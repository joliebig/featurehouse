{-# LANGUAGE FlexibleContexts #-}
-- |
-- Maintainer: Henning Guenther, Martin Wegner, Stephan Friedrichs
--
-- This module contains a monad encapsulating the entire status of the distributor, so that it
-- is completely free of side-effects. As a consequence, dumping the 'ServerState' gives all
-- information about why the distributor has made any decision.
module Barracuda.ServerState (
	-- * Types
	ServerMonad,
	ServerState(..),
	CertificateCallback,
	CertificateRequest,
	Accessor,
	SendCB,
	-- * Monadic operations
	-- ** Running the ServerMonad
	runServerMonad,
	-- ** Retrieving IDs
	newMessageId,
	newChannelId,
	-- ** Sending messages
	sendMessage,
	sendMessageBroadcast,
	sendUser,
	sendUserBroadcast,
	-- ** Accessing attributes
	-- *** General accessors
	with,
	query,
	manipulate,
	-- *** Shortcuts
	withCertificateRequests,
	withCertificates,
	withTimeOffsets,
	withChannelList,
	withLocalUserInfo,
	withPendingAck,
	withPendingAnonymous,
	withPendingKey,
	withPendingRoute,
	withRandom,
	newRandom,
	withRoutingTable,
	-- ** Other accessors
	rootCertificate,
	curTime,
	shutdown,
	-- ** Auxiliary functions
	floodAgain,
	isLocal,
	partitionLocals,
	-- * Logging and debugging
	btrace,
	berror
) where

import Barracuda.CertificateList hiding (certificates)
import Barracuda.ChannelList as CL
import Barracuda.RoutingTable as RT
import Barracuda.LocalUserInfo
import Barracuda.PendingAck as PAck
import Barracuda.PendingRoute as PRoute
import Barracuda.PendingAnonymous as PAnon
import Barracuda.PendingKey as PKey
import Barracuda.GUI.ServerInterface hiding (sendMessage)
import Barracuda.Utils
import Control.Monad.State hiding (mapM_)
import Prelude hiding (mapM_)
import Data.Foldable
import Data.List as List
import Data.Map as Map
import Data.Monoid
import Data.Sequence as Seq
import Data.Set as Set
import Data.Time.Clock
import Data.Word
import Network.AdHoc.UserID
import Network.AdHoc.Channel
import Network.AdHoc.Message
import Network.AdHoc.MessageID
import Network.AdHoc.Routing
import Network.AdHoc.Signature
import Network.GnuTLS.X509
import Network.Socket hiding (Debug, shutdown)
import System.IO
import System.Random as Random
import System.Posix.Unistd

-- | Queue that holds the messages to be sent to the network.
type OutQueue = Seq (SockAddr, InternalMessage)
-- | Queue that holds the 'ControlResponse's to be sent to locally connected users.
type UserQueue = Seq (UserID, ControlResponse)

-- | States how severe a given message is
data Severity
	= Debug	-- ^ The message is only relevant for debugging purposes
	| Error	-- ^ The message informs about an error that should not happen
	deriving (Show,Eq)

-- | Queue that contains output messages.
type PrintQueue = Seq (Severity, String)
-- | Required callback type for functions that want to be called when open certificate requests have been answered.
type CertificateCallback = Map UserID Certificate -> ServerMonad ()
-- | Holds one certificate request. The first set holds all 'UserID's we wait for a certificate from,
--   the second map holds the already satisfied certificates and the third element is the callback
--   to be called when all requests are satisfied.
type CertificateRequest = (Set UserID, Map UserID Certificate, CertificateCallback)

-- | A 'Monad' ('MonadState') wrapping the 'ServerState' and queues for output.
data ServerMonad a = ServerMonad (HostAddress -> Certificate -> UTCTime -> ServerState -> (PrintQueue, OutQueue, UserQueue, ServerState, a))

-- | The state of the server. It holds all relevant data structures that store on the one hand
--   information about the network and on the other hand the internal state of certain data such as
--   open certificate requests and different structures waiting for different events (Pending*).
data ServerState = ServST
	{ routingTable        :: SimpleRT -- ^ The currently known routing table to be used to route messages.
	, chanList            :: ChannelMap -- ^ The list of currently known public and private channels.
	, localUserInfo       :: LocalUserInfo -- ^ Stores information ('UserID', 'PrivateKey', joined channels and a handle to send messages to the GUI of the user) about all locally connected users.
	, certificates        :: CertificateList -- ^ Stores all known certificates.
	, certificateRequests :: [ CertificateRequest ] -- ^ Stores a list of 'CertificateRequest's waiting for certain certificates.
	, userTimeOffsets     :: Map.Map UserID (UTCTime, UTCTime) -- ^ The time offsets to remote users.
	, randGen             :: StdGen -- ^ Stores a random number generator.
	, pendingAck          :: PendingAck InternalSignature -- ^ Stores messages that an ACK is awaited for.
	, pendingAnonymous    :: PendingAnonymous -- ^ Stores anonymous messages to be monitored to be actually posted to the anonymous channel.
	, pendingKey          :: PendingKey -- ^ Stores messages to be de- or encrypted that are waiting for a shared key of a channel.
	, pendingRoute        :: PendingRoute InternalSignature -- ^ Stores messages that are waiting for a route to their destinations.
	, msgIds              :: [MessageID] -- ^ An infinite list of unused 'MessageID's.
	, channelIdCounter    :: Integer -- ^ Counter for the 'ChannelID's we use.
	, infrastructure      :: Maybe (Set SockAddr) -- ^ Maybe stores a set of peers we are (only) connected with.
	, floodBuffer         :: [(UserID, MessageID)] -- ^ Stores information about last received flood messages.
	, nextRouting         :: UTCTime -- ^ Timestamp when routing messages have to be sent the next time.
	, verbose             :: Bool -- ^ Whether to be verbose or not in output.
	, localhost           :: String -- ^ Local hostname.
	, localhostAddr       :: SockAddr -- ^ The local ip and port.
	, wantsShutdown       :: Bool   -- ^ The server wants to be shut down. Access this flag by 'shutdown' rather than directly.
	}

instance Monad ServerMonad where
	return x = ServerMonad (\_ _ _ st -> (Seq.empty, Seq.empty, Seq.empty, st, x))
	(ServerMonad f) >>= g = ServerMonad (\bcast cert time st -> let
		(printSeq, outSeq, clSeq, nst, res) = f bcast cert time st
		ServerMonad f2 = g res
		(printSeq2, outSeq2, clSeq2, nnst, res2) = f2 bcast cert time nst
		in (printSeq >< printSeq2, outSeq >< outSeq2, clSeq >< clSeq2, nnst, res2)
		)

instance MonadState ServerState ServerMonad where
	get = ServerMonad (\_ _ _ st -> (Seq.empty, Seq.empty, Seq.empty, st, st))
	put nst = ServerMonad (\_ _ _ _ -> (Seq.empty, Seq.empty, Seq.empty, nst, ()))

-- | Creates the initial state of the "ServerState", takes the current time, a
--   random number generator to be used subsequently, the 'SockAddr' of the host
--   we are running on and a boolean signalling whether to be verbose or not.
initialState :: UTCTime -> StdGen -> Bool -> String -> SockAddr -> ServerState
initialState now gen v host addr = let (gen1, gen2) = Random.split gen in ServST
	{ routingTable        = RT.empty
	, chanList            = mempty
	, localUserInfo       = Map.empty
	, certificates        = Map.empty
	, certificateRequests = []
	, userTimeOffsets     = Map.empty
	, randGen             = gen1
	, pendingAck          = PAck.empty
	, pendingAnonymous    = PAnon.empty
	, pendingKey          = PKey.empty
	, pendingRoute        = PRoute.empty
	, msgIds              = scrambler 100 gen2 -- scramble order of ids to prevent
	, channelIdCounter    = 0                  -- attacks, e.g. on ANONYMOUS
	, infrastructure      = Nothing
	, floodBuffer         = []
	, nextRouting         = now
	, verbose             = v
	, localhost           = host
	, localhostAddr       = addr
	, wantsShutdown       = False
	}

-- | Generates a new unique 'MessageID'.
newMessageId :: ServerMonad MessageID
newMessageId = with (msgIds) (\newIds state -> state { msgIds = newIds }) (\list -> (tail list, head list))

-- | Retrieves the broadcast ip address.
getBroadcastAddress :: ServerMonad HostAddress
getBroadcastAddress = ServerMonad (\bcast _ _ st -> (Seq.empty, Seq.empty, Seq.empty, st, bcast))

-- | Generates a new unique 'ChannelID'.
newChannelId :: ServerMonad ChannelID
newChannelId = do
	st <- get
	let val = (channelIdCounter st) + 1
	put (st {channelIdCounter = val})
	host <- gets localhost
	return (ChannelID (show val) host)

-- | Queues a message to be sent to the network.
sendMessage :: SockAddr -> InternalMessage -> ServerMonad ()
sendMessage addr msg = ServerMonad (\_ _ _ st -> (Seq.empty, Seq.singleton (addr, msg), Seq.empty, st, ()))

-- | Takes a message to be broadcastet to the network and sends it.
sendMessageBroadcast :: InternalMessage -> ServerMonad ()
sendMessageBroadcast msg = do
	st <- get
	case infrastructure st of
		Nothing -> do    -- We're not in infrastrucure-mode, so broadcast the message to everyone
			bcast <- getBroadcastAddress
			sendMessage (SockAddrInet 8888 bcast) msg
		Just hosts -> do -- We are in infrastructure-mode, send explicit messages to everyone we know
			localhost <- gets localhostAddr -- make sure, we receive our own broadcast
			mapM_ (\host -> sendMessage host msg) (Set.insert localhost hosts)

-- | Sends the given 'ControlResponse' to the given user's GUI.
sendUser :: UserID -> ControlResponse -> ServerMonad ()
sendUser user msg = ServerMonad (\_ _ _ st -> (Seq.empty, Seq.empty, Seq.singleton (user, msg), st, ()))

-- | Sends the given 'ControlResponse' to the GUIs of all connected users.
sendUserBroadcast :: ControlResponse -> ServerMonad ()
sendUserBroadcast msg = do
	locals <- withLocalUserInfo $ query users
	mapM_ (flip sendUser msg) locals

-- | Returns the current time.
curTime :: ServerMonad UTCTime
curTime = ServerMonad (\_ _ time st -> (Seq.empty, Seq.empty, Seq.empty, st, time))

-- | Returns the root certificate used to verify all other certificates.
rootCertificate :: ServerMonad Certificate
rootCertificate = ServerMonad (\_ cert _ st -> (Seq.empty, Seq.empty, Seq.empty, st, cert))

-- | Shut down the server. This is not rather done after processing the current 'DistributorMsg'
--   than immediately.
shutdown :: ServerMonad ()
shutdown = do
	btrace "Requesting server shutdown"
	modify (\st -> st { wantsShutdown = True })

-- | Such a function has to be provided to 'runServerMonad' in order to enable it
--   to perform necessary network output.
type SendCB = [MessageID]  -- ^ An infinite list of unused 'MessageID's.
	-> SockAddr        -- ^ The target network address.
	-> InternalMessage -- ^ This message has to be sent.
	-> IO [MessageID]  -- ^ Another infinite list of unused 'MessageID's. The ones used must be removed.

-- | This function actually runs the entire 'ServerMonad' structure. Given some
--   configurational parameters and a callback to perform network output, it takes
--   a list of timestamped instructions and performs the resulting IO.
runServerMonad :: SendCB               -- ^ A send callback that performs pending network operations.
	-> Bool                        -- ^ Indicates weather we're verbosed or not.
	-> HostAddress                 -- ^ The broadcast address to use
	-> Certificate                 -- ^ The root certificate for the network.
	-> PortNumber		       -- ^ The portnumber, used for hostname generation
	-> [(UTCTime, ServerMonad ())] -- ^ The actual list of timestamped operations to perform.
	-> IO ()
runServerMonad send v bcast cert pn xs = do
	gen <- newStdGen
	now <- getCurrentTime
	host <- getSystemID >>= return.nodeName
	certDir <- getCertificateDirectory
	loadedCerts <- loadCertificateList certDir
	localhost <- inet_addr "127.0.0.1"
	let state = initialState now gen v (host ++ (if pn == 8888 then "" else '_':show pn)) (SockAddrInet pn localhost)
	runServerMonad' send bcast cert xs (state { certificates = loadedCerts })

runServerMonad' :: SendCB -> HostAddress -> Certificate -> [(UTCTime,ServerMonad ())] -> ServerState -> IO ()
runServerMonad' _ _ _ [] st = do
	certDir <- getCertificateDirectory
	dumpCertificateList (certificates st) certDir
runServerMonad' send bcast cert ((time, ServerMonad f):xs) st = let
	(print, out, out_user, nst, _) = f bcast cert time st
	in do
		mapM_ (\(sev, text) -> hPutStrLn (case sev of
			Debug -> stdout
			Error -> stderr) text) (viewl print)
		nids <- foldlM (\ids -> uncurry (send ids)) (msgIds nst) (viewl out)
		mapM_ (\(user, msg) -> userSend user (localUserInfo nst) msg) (viewl out_user)
		runServerMonad' send bcast cert (if wantsShutdown nst then [] else xs) (nst {msgIds = nids})

-- | Type specification for accessor functions (i. e. functions that work on the data structures in the "ServerState").
type Accessor a b = (a -> (a,b)) -> ServerMonad b

-- | The general accessor for The 'ServerState', that makes its attributs available in a
--   monadic calculation. See also: 'query' and 'manipulate', they make things easier.
with :: (ServerState -> a)                   -- ^ A getter function.
	-> (a -> ServerState -> ServerState) -- ^ The setter function.
	-> Accessor a b                      -- ^ Allows monadic access according to the callbacks above.
with getter setter f = do
	st <- get
	let (new,res) = f (getter st)
	put (setter new st)
	return res

-- | Gives access for querying and manipulating the routing table.
withRoutingTable :: Accessor SimpleRT b
withRoutingTable = with routingTable (\rt st -> st {routingTable = rt})

-- | Gives access for querying and manipulating the channel list.
withChannelList :: Accessor ChannelMap b
withChannelList = with chanList (\cl st -> st {chanList = cl})

-- | Gives access for querying and manipulating the local user info.
withLocalUserInfo :: Accessor LocalUserInfo b
withLocalUserInfo = with localUserInfo (\li st -> st {localUserInfo = li})

-- | Gives access for querying and manipulating the pending ack data structure.
withPendingAck :: Accessor (PendingAck InternalSignature) b
withPendingAck = with pendingAck (\pa st -> st {pendingAck = pa})

-- | Gives access for querying and manipulating the pending anonymous data structure.
withPendingAnonymous :: Accessor PendingAnonymous b
withPendingAnonymous = with pendingAnonymous (\pa st -> st {pendingAnonymous = pa})

-- | Gives access for querying and manipulating the pending key data structure.
withPendingKey :: Accessor PendingKey b
withPendingKey = with pendingKey (\pk st -> st {pendingKey = pk})

-- | Gives access for querying and manipulating the pending route data structure.
withPendingRoute :: Accessor (PendingRoute InternalSignature) b
withPendingRoute = with pendingRoute (\pr st -> st {pendingRoute = pr})

-- | Gives access for querying and manipulating the random generator.
withRandom :: Accessor (StdGen) b
withRandom = with randGen (\gen st -> st {randGen = gen})

-- | Returns a new random generator
newRandom :: ServerMonad StdGen
newRandom = withRandom (Random.split)

-- | Gives access for querying and manipulating the certificate list.
withCertificates :: Accessor CertificateList b
withCertificates = with certificates (\lst st -> st {certificates = lst})

-- | Gives access for querying and manipulating the certificate request list.
withCertificateRequests :: Accessor [ CertificateRequest ] b
withCertificateRequests = with certificateRequests (\reqs st -> st {certificateRequests = reqs})

-- | Gives access for querying and manipulating the user time offset list.
withTimeOffsets :: Accessor (Map UserID (UTCTime, UTCTime)) b
withTimeOffsets = with userTimeOffsets (\to st -> st {userTimeOffsets = to})

-- | Helper to wrap modifying \"non-monad functions\" to work with the 'ServerMonad'.
manipulate :: (a -> a) -> (a -> (a,()))
manipulate f x = (f x,())

-- | Helper to wrap querying \"non-monad functions\" to work with the 'ServerMonad'.
query :: (a -> b) -> (a -> (a,b))
query f x = (x,f x)

-- | Checks whether a flood message identified by the given 'UserID' and 'MessageID' was already received before.
floodAgain :: UserID -> MessageID -> ServerMonad Bool
floodAgain user msgid = do
	st <- get
	if (user,msgid) `List.elem` (floodBuffer st)
		then return False
		else put (st {floodBuffer = (user,msgid):(List.take 9 $ floodBuffer st)}) >> return True

-- | Checks whether the given 'UserID' identifies a locally connected user or not.
isLocal :: UserID -> ServerMonad Bool
isLocal user = do
	st <- get
	return $ Map.member user (localUserInfo st)

-- | Splits a given set of users into (locals,non-locals)
partitionLocals :: Set UserID -> ServerMonad (Set UserID,Set UserID)
partitionLocals users = do
	locals <- get >>= return.keysSet.localUserInfo
	return $ Set.partition (\el -> Set.member el locals) users

-- | Used for rather verbosed output, for example for keeping track of an attribute. It
--   only performs the output, if 'verbose' is activated. For error messages use
--   'berror', to be sure it's printed!
btrace :: String -> ServerMonad ()
btrace text = gets verbose >>= \v -> if v then bprint Debug text else return ()

-- | Used for error output. This functions also performs output if 'verbose' is deactivated.
--   See also: 'btrace'.
berror :: String -> ServerMonad ()
berror text = bprint Error text

-- | Appends a text to be printed and the concerning 'Severity' to the 'PrintQueue'.
bprint :: Severity -> String -> ServerMonad ()
bprint sev text = ServerMonad (\_ _ time st ->
	(Seq.singleton (sev, show time ++ "\t" ++ text), Seq.empty, Seq.empty, st, ()))

