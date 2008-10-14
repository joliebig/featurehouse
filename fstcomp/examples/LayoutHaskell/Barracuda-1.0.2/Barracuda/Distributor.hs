{-# LANGUAGE FlexibleContexts #-}
-- |
-- Maintainer: Henning Guenther, Martin Wegner
--
-- This is the main component of Barracuda. It contains the logic of messages flow in
-- the application. In this case, \'message\' does means 'DistributorMsg's rather than
-- only protocol messages.
module Barracuda.Distributor
	( DistributorMsg(..)
	, DistributorChan
	, processMessage
	) where

import Control.Monad.State hiding (mapM_,forM_)
import Control.Concurrent.Chan
import Data.Array (Array, listArray, bounds, (!))
import qualified Data.ByteString as BS
import Data.Time.Clock
import Data.Word
import Data.Foldable (foldl,forM_)
import Data.Map as Map hiding (map, (\\), (!))
import qualified Data.Set as Set
import Data.List as List ((\\),partition,filter,null)
import Data.Maybe (listToMaybe)
import Network.BSD
import Network.GnuTLS.X509
import qualified Network.GnuTLS as GnuTLS
import Network.Socket hiding (shutdown)
import Prelude hiding (foldl)
import System.Random
import System.IO
import Text.XML.HaXml.Parse(xmlParse')
import Text.XML.HaXml.Pretty

import Barracuda.ServerState
import Barracuda.RoutingTable as RT
import Barracuda.CertificateList (verify)
import Barracuda.ChannelList as CL
import Barracuda.PendingAck as PAck
import Barracuda.PendingRoute as PRoute
import Barracuda.PendingAnonymous as PAnon
import Barracuda.PendingKey as PKey
import Barracuda.LocalUserInfo as LocInf
import Barracuda.TimedCollection
import Barracuda.GUI.ServerInterface hiding (sendMessage)
import Network.AdHoc.Encryption
import Network.AdHoc.Message
import Network.AdHoc.MessageID
import Network.AdHoc.ParserStrict
import Network.AdHoc.Generator
import Network.AdHoc.Routing
import Network.AdHoc.UserID
import Network.AdHoc.Signature
import Network.AdHoc.Channel

-- | The Distributor is mainly controlled via distributor messages. They can be created
--   and be given to the 'processMessage' function to be processed on the "ServerState".
data DistributorMsg
	= ProtMsg SockAddr String -- ^ A message received from the network.
	| NewGUI UserID Certificate PrivateKey (ControlResponse -> IO ()) -- ^ Signals that a new GUI was created.
	| GUIMsg UserID ControlMessage -- ^ 'ControlMessage's from one of the connected GUIs.
	| TimeMsg -- ^ A timer message constantly pinging the Distributor to trigger certain periodic actions like sending HELLOs, etc.
	| InfraMsg (Maybe (Set.Set SockAddr)) -- ^ A message containing a set of 'SockAddr'esses being the list of known peers. Used for infrastructure mode.

-- | Through this channel the Distributor receives the 'DistributorMsg's.
type DistributorChan = Chan (UTCTime, DistributorMsg)

-- ------------------------
-- - PROCESSING FUNCTIONS -
-- ------------------------

type ObscureMessage = Either (String, [ Attachment ], UTCTime) (UserID, RSAEncrypted String)

-- | Main routine of the Distributor: 'processMessage' takes a 'DistributorMsg' and processes it.
--   This includes: Periodic sending of HELLOs and ROUTINGs triggered by timer messages,
--   processing all received protocol messages and updating the stored information about the network
--   in the "ServerState" respectively sending further messages (e. g. to the remaining non-local receivers).
--   And additionally it connects the protocol backend of the program to the connected GUIs. Local users
--   are registered to the Distributor, and the Distributor distributes all incoming messages for local users
--   to those connected users respectively taking messages from them to be sent to the network.
processMessage :: DistributorMsg -> ServerMonad ()
--
-- Infrastructure message
--
processMessage (InfraMsg set) = do
	btrace $ "Infrastructure: " ++ show set
	modify (\st -> st {infrastructure = set})
--
-- Timer message
--
processMessage TimeMsg = do
	localUsers <- withLocalUserInfo $ query $ LocInf.users
	-- Send hello message
	unless (Set.null localUsers) $ do
		greeting <- withRandom (\gen -> let (i, g) = randomR (bounds hellos) gen in (g, i)) >>= return.(hellos!)
		sendMessageBroadcast (Hello (Set.toList localUsers) 1 (Just greeting))
	time <- curTime
	-- Remove old entries from the routing table
	withRoutingTable $ manipulate $ deleteBefore (addUTCTime (-6) time)
	-- Distribute channel information so that we can update the users in the anonymous channel for all GUIs
	-- (users might have been added or have timeouted)
	distributeChannelInformation
	-- Shuffle messages that await routing etc.
	messageShuffle
	-- Send routing messages
	timedSendRouting
	-- Purge anonymous messages that have timeouted
	withPendingAnonymous $ manipulate $ PAnon.purgeZeroTTL
	-- Try to resend Anonymous messages that were not posted after a timeout of 60 seconds
	anon <- withPendingAnonymous $ PAnon.purgeNotPosted time 60
	-- Count down TTL of remaining anonymous messages
	let ranon = map (\anonMsg -> anonMsg { ttl = if (ttl anonMsg) > 60 then (ttl anonMsg) - 60 else 0 }) anon
	-- Try to resend anonymous messages
	mapM_ sendAnonymous ranon
	-- re-announce channels
	channelsToAnnounce <- withChannelList $ CL.announcePurge localUsers (addUTCTime (-60) time) time
	mapM_ announceChannel channelsToAnnounce
	-- re-request keys for private channels that have not yet arrived here
	channelsToRequest <- withChannelList $ query $ CL.unknown time 60
	mapM_ (\(cname, cid) -> requestPrivChanKey cname cid) channelsToRequest
--
-- Protocol message
--
processMessage (ProtMsg from str) = do
	st <- get
	case xmlParse' (show from) str >>= parseMessage (verify (certificates st)) of
		Left err -> berror $ "Failed to parse message: " ++ err ++ "Failed message: " ++ str
		Right msg -> do
			case (infrastructure st) of
				Nothing -> processProtocolMessage from msg
				Just peers -> if Set.member from peers
					then processProtocolMessage from msg
					else btrace $ "Dropped packet from " ++ show from ++ " due to infrastructure"
--
-- GUI creation and close messages
--
processMessage (NewGUI uid cert pk handle) = do
	withLocalUserInfo $ manipulate $ Map.insert uid (Set.empty, pk, handle)
	withCertificates  $ manipulate $ Map.insert uid cert
	distributeChannelInformation
processMessage (GUIMsg user CMClose) = do
	userInfo <- withLocalUserInfo $ query $ Map.lookup user
	case userInfo of
		Just (channels, privkey, _) -> do
			mapM_ (\(cname, cid) -> leaveUser user privkey cname cid) (Set.toList channels)
			withLocalUserInfo $ manipulate $ Map.delete user
			newUsers <- withLocalUserInfo $ query id
			when (Map.null newUsers) shutdown
		Nothing -> return () -- should not happen
--
-- GUI message
--
processMessage (GUIMsg user (WantJoin cname cid)) = do
	userInfo <- withLocalUserInfo $ query $ Map.lookup user
	case userInfo of
		Just (channels, privkey, handle) -> do
			isPrivate <- withChannelList $ query $ CL.isPrivate cname cid
			case isPrivate of
				Just True -> return ()
				_ -> do
					withLocalUserInfo $ manipulate $ Map.insert user (Set.insert (cname, cid) channels, privkey, handle)
					withChannelList $ manipulate $ CL.join user cname cid
					distributeChannelInformation
			joinUser user privkey cname cid
		Nothing -> berror $ "Got GUIMsg from user " ++ (show user) ++ " that I do not know?!"
processMessage (GUIMsg user (WantLeave cname cid)) = do
	userInfo <- withLocalUserInfo $ query $ Map.lookup user
	case userInfo of
		Just (channels, privkey, handle) -> do
			withLocalUserInfo $ manipulate $ Map.insert user (Set.delete (cname, cid) channels, privkey, handle)
			leaveUser user privkey cname cid
			withChannelList $ manipulate $ leave user cname cid
			distributeChannelInformation
		Nothing -> berror $ "Got GUIMsg from user " ++ (show user) ++ " that I do not know?!"
processMessage (GUIMsg sender (SendMsg cname cid text attachments)) = do
	time <- curTime
	if mkChannelName "Anonymous" == cname
		then sendAnonymous (PendingAnonymousMessage 360 sender text attachments time)
		else do
			privKey <- withLocalUserInfo $ query $ privateKey sender
			-- query users in channel
			usersChannel <- withChannelList $ query $ CL.usersChannel cname cid
			btrace $ "Received message to be sent from local user "++(show sender)
			-- partition them into local and non-local users
			(localRecv, nonLocalRecv) <- partitionLocals usersChannel
			btrace $ "Forwarding to: "++(show localRecv)++", "++(show nonLocalRecv)
			case privKey of
				Just rprivKey -> do
					unless (Set.null nonLocalRecv) $ sendMsgToChannel sender rprivKey cname cid (Set.toList nonLocalRecv) text attachments
					mapM_ ((flip sendUser) (Receive cname cid (Just sender) text attachments time False)) (Set.toList localRecv)
				Nothing -> berror $ "Got GUIMsg from user " ++ (show sender) ++ " that I do not have a private key of?!"
processMessage (GUIMsg user (CreateChannel name desc invite)) = do
	-- check whether channel exists
	channelExists <- withChannelList $ query $ CL.exists name
	if channelExists
		then
			-- tell user that channel already exists
			sendUser user (ErrGeneral MessageWarning "Channel name already occupied"
				("A channel with the name '" ++ show name
				++ "' already exists, so I'm unable to create the channel."))
		else do
			case validateChannelName name of
				Just err -> sendUser user (ErrGeneral MessageInfo "Unable to create channel" err)
				Nothing  -> do -- in this case, Nothing means that everything's alright
					userInfo <- withLocalUserInfo $ query $ Map.lookup user
					case userInfo of
						Just (channels, privKey, handle) -> do
							time <- curTime
							channelID <- newChannelId
							msgID <- newMessageId
							(private, key, channelUsers) <- case invite of
								Just users -> do
									key <- withRandom (\gen -> let (res, ngen) = generateDESKey gen in (ngen, res))
									return (True, Just key, Set.toList (Set.insert user users))
								Nothing -> return (False, Nothing, [ user ])
							-- add channel to ChannelList
							btrace $ "Inserting channel into ChannelList ..."
							withChannelList $ manipulate $ CL.update time name channelID desc private channelUsers
							-- if channel is private insert the key
							case key of
								Just rkey -> withChannelList $ manipulate $ CL.insert name channelID rkey
								Nothing -> return ()
							-- announce channel
							sendMessageBroadcast (Flood (Routed 48 user msgID (Channel name channelID desc channelUsers private) (Left privKey)))
							-- add channel to LocalUserInfo for the user
							withLocalUserInfo $ manipulate $ Map.insert user (Set.insert (name, channelID) channels, privKey, handle)
							distributeChannelInformation
						Nothing -> berror $ "Got GUIMsg from user " ++ (show user) ++ " that I do not know?!"
processMessage (GUIMsg sender (Authorize user cname cid)) = do
	privKey <- withLocalUserInfo $ query $ privateKey sender
	case privKey of
		Just rprivKey -> do
			-- join the user, then query updated information about the channel and re-send announcement
			withChannelList $ manipulate $ CL.join user cname cid
			channelInfo <- withChannelList $ query $ CL.lookup cname cid
			case channelInfo of
				Just (private, desc, users) -> do
					msgID <- newMessageId
					sendMessageBroadcast (Flood (Routed 48 sender msgID (Channel cname cid desc (Set.toList users) private) (Left rprivKey)))
				Nothing -> return ()
		Nothing -> berror $ "Got GUIMsg from user " ++ (show user) ++ " that I do not know?!"

messageShuffle :: ServerMonad ()
messageShuffle = do
	time <- curTime
	-- Get messages that were not successfully sent to the next hop
	noAck <- withPendingAck (purgeNoAck time 2) -- Wait 2 seconds for an ACK message
	-- And reinsert	them into the PendingRoute structure
	withPendingRoute $ manipulate $
		flip (foldl (\pr (routed,arrived,tried) -> PRoute.reinsert arrived routed tried pr)) noAck
	-- Get the routing table
	rt <- withRoutingTable $ query id
	-- Find new routes for pending messages
	newRoutes <- withPendingRoute $ routeAndDelete rt
	-- And send the messages along them
	mapM_ (\(addr,rt,tried,recv_time) -> resend addr rt tried recv_time) newRoutes

sendRouting :: ServerMonad ()
sendRouting = do
	-- Find all neighbors
	neighborhood <- withRoutingTable $ query neighbors
	users <- withLocalUserInfo $ query users
	-- And send them routing messages
	mapM_ (\addr -> do
		routes <- withRoutingTable $ query $ mergeRoutesFor addr users
		sendMessage addr (Routing (Map.toList routes))
		) (Set.toList neighborhood)

nextRoutingInterval :: ServerMonad ()
nextRoutingInterval = do
	now <- curTime
	interval <- withRandom (\gen -> let (res,ngen) = randomR (8,10) gen in (ngen,res))
	st <- get
	put $ st { nextRouting = (fromInteger interval) `addUTCTime` now }

timedSendRouting :: ServerMonad ()
timedSendRouting = do
	now <- curTime
	st <- get
	when (now >= nextRouting st) $ do
		sendRouting
		nextRoutingInterval
		rt <- withRoutingTable $ query id
		btrace $ show rt

processProtocolMessage :: SockAddr -> ExternalMessage -> ServerMonad ()
processProtocolMessage from msg = case msg of
	Hello users vers greet -> do
		time <- curTime
		lusers <- withLocalUserInfo $ query $ LocInf.users
		when (Set.null $ Set.intersection lusers (Set.fromList users))
			$ withRoutingTable $ manipulate $ RT.hello time from users
	Ack sender msgid -> do
		-- The PendingAck structure needs to be updated
		withPendingAck $ manipulate $ ack sender msgid from
	Routing routes -> do
		time <- curTime
		withRoutingTable $ manipulate $ RT.routing time from routes
		rt <- withRoutingTable $ query id
		btrace $ "Received routing message " ++ show rt
	Flood (Routed ttl sender msgid fc sig) -> do
		let processCall = do
			again <- floodAgain sender msgid
			when (again && ttl >= 1) $ do
				processFlood sender msgid fc sig
				when (ttl > 1) $ sendMessageBroadcast (Flood (Routed (ttl-1) sender msgid fc (toInternal sig)))
		case sig of
			Nothing -> processCall
			Just (_, sigStatus) -> case sigStatus of
				SignatureWrong -> btrace $ "Received flood message with wrong signature from " ++ show sender
				_ -> processCall
	Target rt@(Routed ttl sender msgid tc sig) -> do
		let processCall = giveAck from sender msgid >> case tc of
			Nack nrt@(Routed _ receiver _ _ _) -> do
				local <- isLocal receiver
				if local
					then do
						-- We just try to resend the message 'as-is' maybe it will success now
						processTarget nrt
					else case decrementTTL rt of
						Nothing -> return () -- Time's up, baby
						Just rrt -> sendRouted (fmap Right rrt)
			_ -> do
				processTarget rt
		case sig of
			Nothing -> processCall
			Just (_, sigStatus) -> case sigStatus of
				SignatureWrong -> btrace $ "Received message with wrong signature from "++show sender++" for message "++msgid
				_ -> processCall
	Obscure rt@(Routed ttl receiver msgID obscData _) -> processObscure ttl receiver msgID obscData

processFlood :: UserID -> MessageID -> FloodContent -> ExternalSignature -> ServerMonad ()
processFlood sender _ (Channel cname cid ctitle users private) sig = unless (cname == anonymous) $ do
	btrace $ "Received channel announcement for channel "++(show cname)++" ("++(show cid)++")"
	isPrivate <- withChannelList $ query $ CL.isPrivate cname cid
	-- TODO: a better way to distinct the cases and do the necessary calls may be considered
	case isPrivate of
		Just True -> case sig of -- here we rely on our stored information
			Just (_, sigStatus) -> processChannel sender cname cid ctitle users True sigStatus
			Nothing -> btrace $ "Rejected channel announcement w/o signature" -- announcements of private channels without signature are rejected
		Just False -> processChannel sender cname cid ctitle users False SignatureOK -- for public channels signature does not matter
		Nothing -> if private -- we have no information about the channel, so trust the message
			then case sig of
				Just (_, sigStatus) -> processChannel sender cname cid ctitle users private sigStatus
				Nothing -> btrace $ "Rejected channel announcement w/o signature" -- announcements of private channels without signature are rejected
			else processChannel sender cname cid ctitle users private SignatureOK -- for public channels signature does not matter
processFlood sender _ (Join cname cid) sig = do
	-- is channel private?
	private <- withChannelList $ query $ CL.isPrivate cname cid
	case private of
		Just True -> do
			-- decide whether one of our local users may be able to allow the join:
			usersChannel <- withChannelList $ query $ CL.usersChannel cname cid
			-- only request auth when sender is not yet in the channel:
			unless (Set.member sender usersChannel) $ do
				(localUsers, _) <- partitionLocals usersChannel
				-- only when we have a user, request auth from him
				unless (Set.null localUsers) $ do
						let localUser = Set.findMin localUsers
						btrace $ "Requesting authorization for channel "++(show cname)++" ("++(show cid)++") from local user "++(show localUser)
						sendUser localUser (WantsAuth sender cname cid)
		_ -> do
			withChannelList $ manipulate $ CL.join sender cname cid
			distributeChannelInformation
	cl <- withChannelList $ query id
	btrace $ show cl
processFlood sender _ (Leave cname cid) sig = do
	withChannelList $ manipulate $ CL.leave sender cname cid
	distributeChannelInformation
	cl <- withChannelList $ query id
	btrace $ show cl
processFlood _ _ (Anonymous text attachments msgTime _) sig = do
	btrace $ "Received anonymous message with text '"++text++"' and time "++(show msgTime)
	withPendingAnonymous $ manipulate $ PAnon.posted text msgTime
	time <- curTime -- Mangling foreign timestamps
	sendUserBroadcast (Receive anonymous (ChannelID "Anonymous" "Anonymous") Nothing text attachments time False)

processChannelCallback :: UserID -> ChannelName -> ChannelID -> String -> [ UserID ] -> Bool -> (Certificate -> SignatureStatus) -> CertificateCallback
processChannelCallback sender cname cid ctitle users private verify cert
	= case Map.lookup sender cert of
		Just rcert -> do
			btrace $ "Got certificate for an older announcement, processing it ..."
			processChannel sender cname cid ctitle users private (verify rcert)
		Nothing -> return () -- This case should not happen

processChannel' :: UserID -> ChannelName -> ChannelID -> String -> [ UserID ] -> Bool -> ServerMonad ()
processChannel' sender cname cid ctitle users private = do
	btrace $ "Processing channel announcement for channel " ++ show cname
	-- process announcement
	oldUsersChannel <- withChannelList $ query $ CL.usersChannel cname cid
	-- we do only process the announcement in the following if
	--  - we do not know the channel yet
	--  - the channel is public
	--  - the channel is private and the sender is member in the channel according to our previously stored info
	if (Set.null oldUsersChannel) || (Set.member sender oldUsersChannel) || not private
		then do
			let usersChannel = Set.fromList users
			btrace $ "Users in channel: "++(show usersChannel)
			localUsers <- withLocalUserInfo $ query $ Map.assocs
		 	-- update channels of local users in LocalUserInfo and check whether we have to request the channel key
			when private $ mapM_ (joinUserLocally sender cname cid usersChannel) localUsers
			-- update stored information about the channel
			updateChannelInformation cname cid ctitle private usersChannel
			-- query local users
			localUsers <- withLocalUserInfo $ query $ Map.assocs -- work with up-to-date information
			-- re-send leaves for all channels local users are no longer in
			mapM_ (resendLeave cname cid usersChannel) localUsers
	 		-- re-send JOINs for users that are not in the public channel but that want to be in it
			unless private $ mapM_ (resendJoin cname cid usersChannel) localUsers
			btrace $ "Channel announcement processed."
		else btrace $ "Announcement for channel " ++ show cname ++ " rejected"

processChannel :: UserID -> ChannelName -> ChannelID -> String -> [ UserID ] -> Bool -> SignatureStatus -> ServerMonad ()
processChannel sender cname cid ctitle users private sigStatus = do
	if private
		then case sigStatus of
			SignatureWrong -> btrace $ "Received channel announcement w/ wrong signature" -- channel is private, but signature of announcement is wrong, so drop it
			CertificateMissing verify -> do -- Certificate is missing, hold back announcement
				btrace $ "Got channel announcement for channel " ++ (show cname) ++ ", but I have no cert, deferring ..."
				rl <- randomLocal
				case rl of
					Nothing -> btrace $ "Cannot request certificate to check private channel announcement (note that this is the fault of the shitty protocol)"
					Just (localUser, _) -> requestCertificates localUser (Set.singleton sender) Map.empty
						(processChannelCallback sender cname cid ctitle users private verify)
			-- everything's fine, so process the announcement:
			SignatureOK -> processChannel' sender cname cid ctitle users private
		else processChannel' sender cname cid ctitle users private
	cl <- withChannelList $ query id
	btrace $ show cl

processTarget :: Routed TargetContent ExternalSignature -> ServerMonad()
processTarget rt@(Routed ttl sender msgid tc sig) = do
	case tc of
		GetCertificate for -> processGetCertificate rt for
		Certificate receiver for cert -> processCertificate rt
		GetKey receiver cname cid -> do	
			local <- isLocal receiver
			if local
				then do
					processGetKey sender receiver cname cid
				else case decrementTTL rt of
					Nothing -> giveNack (fmap Right rt)
					Just nrt -> sendRouted (fmap Right nrt)
		Key receiver cname cid ctype key -> do
			privkey <- withLocalUserInfo $ query $ privateKey receiver
			case privkey of
				-- User is local
				Just rprivkey -> case rsaDecrypt rprivkey key of
					Nothing -> berror $ "Couldn't decrypt channel key from "++show sender
					Just rkey -> do
						withChannelList $ manipulate $ CL.insert cname cid rkey
						pendingMsgs <- withPendingKey $ PKey.purge cname cid
						case pendingMsgs of
							Just rpendingMsgs ->
								mapM_ (processPendingKeyMessage rkey) rpendingMsgs
							Nothing -> return ()
				Nothing -> case decrementTTL rt of
					Nothing -> giveNack (fmap Right rt)
					Just nrt -> sendRouted (fmap Right nrt)
		Message _ _ _ _ _ _ -> processMessageMessage rt

processGetCertificate :: Routed TargetContent ExternalSignature -> UserID -> ServerMonad ()
processGetCertificate rt for = do
	btrace $ "Received GetCertificate for local user " ++ (show for) ++ " from " ++ (show (routedUserID rt))
	-- Do we have the certificate?
	qcert <- withCertificates (query $ Map.lookup for)
	case qcert of	
		-- No? Then try to forward the request
		Nothing -> case decrementTTL rt of		-- Can the message be further forwarded?
			Nothing -> giveNack (fmap Right rt)	-- No? Then Nack it
			Just nrt -> sendRouted (fmap Right nrt)	-- Yes? Great, send it to the requested user
		-- Yes? Good, then we can give it to the requester
		Just cert -> do	
			-- Export the certificate and send it to the requester
			mid <- newMessageId
			let cData = case GnuTLS.exportCertificate cert GnuTLS.X509FmtDer of
				Left err -> error ("Internal GnuTLS error: "++show err)
				Right c -> c
			btrace $ "Having the certificate for " ++ (show for) ++ ", answering ..."
			sendRouted (Routed 360 for mid (Certificate [routedUserID rt] for cData) (Right Nothing))

processCertificate :: Routed TargetContent ExternalSignature -> ServerMonad ()
processCertificate rt@(Routed ttl sender msgID (Certificate receivers for cert) sig) = rootCertificate >>= \root -> case (do
	rcert <- GnuTLS.importCertificate cert GnuTLS.X509FmtDer
	isIssuer <- GnuTLS.checkIssuer rcert root
	unless isIssuer (fail "Certificate is not signed by root certificate.")
	return rcert) of
		Left err -> btrace $ "Received bad certificate for " ++ show for ++ ": " ++ show err
		Right rcert -> do
			btrace $ "Got the certificate from "++(show for)
			withCertificates $ manipulate $ Map.insert for rcert
			locals <- withLocalUserInfo $ query $ users
			let remainRecvs = receivers \\ (Set.toList locals)
			unless (List.null remainRecvs) $ sendRouted (Routed ttl sender msgID (Certificate remainRecvs for cert) (toInternal sig))
			checkCertificateRequests for rcert

processGetKeyCallback :: UserID -> PrivateKey -> ChannelName -> ChannelID -> UserID -> Word64 -> CertificateCallback
processGetKeyCallback sender privkey cname cid receiver key certs = do
	btrace $ "Key for channel "++(show cname)++" ("++(show cid)++") was requested from local user "++(show sender)++" ..."
	msgID <- newMessageId
	let cert = Map.lookup receiver certs
	case cert of
		Just rcert -> do
			rand <- newRandom
			btrace $ "Sending key from "++(show sender)++" to "++(show receiver)++" ..."
			sendRouted (Routed 360 sender msgID (Key receiver cname cid CipherDES_CBC (rsaEncrypt rand rcert key)) (Left privkey))
		Nothing -> do return () -- this case should not happen ...

processGetKey :: UserID -> UserID -> ChannelName -> ChannelID -> ServerMonad ()
processGetKey sender receiver cname cid = do
	-- check whether we have the key
	key <- withChannelList $ query $ CL.getKey receiver cname cid
	case key of
		Just rkey -> do
			-- Yes, we have the key, so send him the key
			privKey <- withLocalUserInfo $ query $ LocInf.privateKey receiver
			case privKey of
				Just rprivKey -> do
					-- lookup certificate for receiver
					(usersWOCert, usersWCert) <- seperateUsersWithCert [ sender ]
					let callback = processGetKeyCallback receiver rprivKey cname cid sender rkey
					if Set.null usersWOCert
						-- we have already the certificate
						then do
							callback usersWCert
						-- we do not have it, so start a request
						else do
							requestCertificates receiver usersWOCert usersWCert callback
				Nothing -> return () -- eh, should not happen
		Nothing -> do
			-- we do not have the key?? that's weird but we just don't process the request then
			return ()

processMessageMessage :: Routed TargetContent ExternalSignature -> ServerMonad ()
processMessageMessage rt@(Routed ttl sender msgid msg@(Message receivers cname cid cont time delay) sig) = do
	btrace("Processing message ...")
	-- query users in channel
	usersChannel <- withChannelList $ query $ CL.usersChannel cname cid
	-- is sender member of the channel?
	when (Set.member sender usersChannel) $ do -- yes
		btrace $ "Partitioning users ..."
		-- partition receivers in local and non-local ones
		btrace $ "Receivers: "++(show receivers)
		(localRecv, nonLocalRecv) <- partitionLocals (Set.fromList receivers)
		btrace $ show localRecv
		btrace $ show nonLocalRecv
		-- forward message to any remaining non-local users
		btrace $ "Forwarding to remaining users ..."
		unless (Set.null nonLocalRecv) $ do
				let rt = (Routed ttl sender msgid (Message (Set.toList nonLocalRecv) cname cid cont time (delay + 1)) (toInternal sig))
				case decrementTTL rt of
					Nothing -> giveNack rt
					Just nrt -> sendRouted nrt
		-- calculate time offsets
		currentTime <- curTime
		offset <- withTimeOffsets $ query $ Map.lookup sender
		delayed <- case offset of
			Just (locTime, forTime) -> let offset = abs (diffUTCTime locTime forTime); msgOffset = abs (diffUTCTime currentTime time) in
				if msgOffset < offset
					then (withTimeOffsets $ manipulate $ Map.insert sender (currentTime, time)) >> return False
					else return (msgOffset > offset + 10) -- 8 secs
			Nothing -> do
				withTimeOffsets $ manipulate $ Map.insert sender (currentTime, time)
				return (False) -- For the first message we cannot say if it got delayed
		btrace $ "Forwarding to local users ..."
		offset' <- withTimeOffsets $ query $ Map.lookup sender
		let correctedTime = case offset' of -- correct timestamp
			Just (locTime, forTime) -> addUTCTime (diffUTCTime locTime forTime) time --currentTime
			Nothing                 -> currentTime
		mapM_ (processMessageForLocalUser sender correctedTime delayed msg) (Set.toList localRecv)

processMessageForLocalUser :: UserID -> UTCTime -> Bool -> TargetContent -> UserID -> ServerMonad ()
processMessageForLocalUser sender time delayed msg@(Message _ cname cid cont _ delay) receiver = do
	case cont of
		EncryptedMessage text attachments -> do
			havingKey <- (withChannelList $ query $ keyKnown cname cid)
			if havingKey
				-- we have the key, try to decrypt the message:
				then do
					key <- withChannelList $ query $ getKey receiver cname cid
					case key of
						Just rkey -> case decryptMessageContent rkey text attachments of
							Nothing -> berror $ "Failed to decrypt message from "++show sender
							Just (decText,decAttachments) -> sendUser receiver (Receive cname cid (Just sender) decText decAttachments time delayed)
						Nothing -> return () -- somehow we are not allowed to retrieve the key ...
				-- we do not have the key, so defer the message until we have the key
				else withPendingKey $ manipulate $ PKey.insert cname cid (Right (sender, msg, time, delayed))
		UnencryptedMessage text attachments -> sendUser receiver (Receive cname cid (Just sender) text attachments time delayed)

decryptMessageContent :: Word64 -> Encrypted String -> [ EncryptedAttachment ] -> Maybe (String, [ Attachment ])
decryptMessageContent key text a = do
	rtext <- decrypt key text
	rattach <- mapM (decryptAttachment key) a
	return (rtext,rattach)

processObscure :: TTL -> UserID -> MessageID -> RSAEncrypted String -> ServerMonad ()
processObscure ttl receiver msgID obscData = do
	-- is obscure addressed to local user?
	privkey <- withLocalUserInfo $ query $ LocInf.privateKey receiver
	case privkey of
		-- Yes, it is, so process it
		Just rprivkey -> case rsaDecrypt rprivkey obscData of -- decrypt content
			Nothing -> berror $ "Failed to decrypt obscure message"
			Just decrypted -> case xmlParse' "ObscureMessage" decrypted >>= parseInnerMessage of
				Left err -> berror $ "Failed to parse inner message: " ++ err ++ " failed message: "++decrypted
				Right msg -> case msg of
					Left (receiver, innerData) -> do
						msgID <- newMessageId
						sendObscure (Routed 32 receiver msgID innerData ())
					Right (time, text, attachments) -> do
						sendAnonymousMessage 360 text attachments time
		-- No
		Nothing -> do
			let rt = Routed ttl receiver msgID obscData ()
			-- forward message
			case decrementTTL rt of
				Nothing -> return () -- It's over here
				Just nrt -> sendObscure rt

-- ----------------------------------
-- - PROCESSING HELPER FUNCTIONS - --
-- ----------------------------------

-- | Request the shared key for a private channel without given two users, one that requests the key, one that has the key
requestPrivChanKey :: ChannelName -> ChannelID -> ServerMonad ()
requestPrivChanKey cname cid = do
	btrace $ "Requesting key for private channel " ++ show cname ++ " (" ++ show cid ++ "):"
	channelUsers <- withChannelList $ query $ CL.usersChannel cname cid
	btrace $ "Users in channel:"
	btrace $ show channelUsers
	(localUsers, nonLocalUsers) <- partitionLocals channelUsers
	btrace $ show localUsers ++ ", " ++ show nonLocalUsers
	unless (Set.null localUsers || Set.null nonLocalUsers) $ do
		let sender = Set.findMin localUsers
		let receiver = Set.findMin nonLocalUsers
		privKey <- withLocalUserInfo $ query $ privateKey sender
		case privKey of
			Just rprivKey -> requestPrivChanKey' cname cid receiver sender rprivKey
			Nothing -> return () -- this case should not happen

-- | Request the shared key for a private channel
requestPrivChanKey' :: ChannelName -> ChannelID -> UserID -> UserID -> PrivateKey -> ServerMonad ()
requestPrivChanKey' cname cid announceSender user privKey = do
	btrace $ "Requesting key for private channel " ++ show cname ++ " (" ++ show cid ++ ") ..."
	-- check whether channel is private (just a check)
	isPrivate <- withChannelList $ query $ CL.isPrivate cname cid
	-- check whether we have to request the key
	havingKey <- withChannelList $ query $ CL.keyKnown cname cid
	case isPrivate of
		Just True -> unless havingKey $ do
			time <- curTime
			msgID <- newMessageId
			withChannelList $ manipulate $ CL.request time cname cid
			sendRouted (Routed 360 user msgID (GetKey announceSender cname cid) (Left privKey))
		Nothing -> return ()

joinUserLocally :: UserID -> ChannelName -> ChannelID -> Set.Set UserID -> (UserID, SingleUserInfo) -> ServerMonad ()
joinUserLocally sender cname cid cusers (user, (channels, pk, h))
	= when (Set.member user cusers && not (Set.member (cname, cid) channels)) $ do
		withLocalUserInfo $ manipulate $ Map.insert user (Set.insert (cname, cid) channels, pk, h)
		requestPrivChanKey' cname cid sender user pk

joinUser :: UserID -> PrivateKey -> ChannelName -> ChannelID -> ServerMonad ()
joinUser user privkey cname cid = do
	msgID <- newMessageId
	sendMessageBroadcast (Flood (Routed 360 user msgID (Join cname cid) (Left privkey)))

leaveUser :: UserID -> PrivateKey -> ChannelName -> ChannelID -> ServerMonad ()
leaveUser user privKey cname cid = do
	btrace $ "Sending leave for user " ++ show user ++ " in " ++ show cname ++ " " ++ show cid
	msgID <- newMessageId
	sendMessageBroadcast (Flood (Routed 360 user msgID (Leave cname cid) (Left privKey)))

resendJoin :: ChannelName -> ChannelID -> Set.Set UserID -> (UserID, SingleUserInfo) -> ServerMonad ()
resendJoin cname cid users (user, (channels, privKey, _))
	= when ((Set.member (cname, cid) channels) && not (Set.member user users)) $ joinUser user privKey cname cid

resendLeave :: ChannelName -> ChannelID -> Set.Set UserID -> (UserID, SingleUserInfo) -> ServerMonad ()
resendLeave cname cid users (user, (channels, privKey, _))
	= when ((Set.member user users) && not (Set.member (cname, cid) channels)) $ leaveUser user privKey cname cid

announceChannel :: (ChannelName, ChannelID, String, Set.Set UserID, Bool) -> ServerMonad ()
announceChannel (cname, cid, desc, localUsers, private) = unless (Set.null localUsers) $ do
	let localUser = Set.findMin localUsers
	channelUsers <- withChannelList $ query $ CL.usersChannel cname cid
	privkey <- withLocalUserInfo $ query $ privateKey localUser
	msgID <- newMessageId
	case privkey of
		Just rprivkey -> sendMessageBroadcast (Flood (Routed 48 localUser msgID (Channel cname cid desc (Set.toList channelUsers) private) (Left rprivkey)))
		Nothing -> return () -- This should not happen

mergeChannel :: ChannelName -> ChannelID -> ChannelID -> String -> Set.Set UserID -> ServerMonad ()
mergeChannel cname ocid ncid ctitle users = do
	(nlocalUsers, _) <- partitionLocals users
	-- announce merged channel
	announceChannel (cname, ncid, ctitle, nlocalUsers, False)
	-- update LocalUserInfo with merged channel id so that we are consistent again
	forM_ nlocalUsers (\user -> do
		userInfo <- withLocalUserInfo $ query $ Map.lookup user
		case userInfo of
			Just (channels, pk, h) -> withLocalUserInfo $ manipulate $ Map.insert user (Set.insert (cname, ncid) (Set.delete (cname, ocid) channels), pk, h)
			Nothing -> return ()
		)

updateChannelInformation :: ChannelName -> ChannelID -> String -> Bool -> Set.Set UserID -> ServerMonad () 
updateChannelInformation cname cid ctitle private users = do
	time <- curTime
	oldChannelInfo <- withChannelList $ query $ CL.lookupPublic cname
	knownUsers <- withRoutingTable $ query $ RT.userList
	localUsers <- withLocalUserInfo $ query $ LocInf.users
	let allUsers = Set.union knownUsers localUsers
	let remUsers = Set.filter (\user -> Set.member user allUsers) users
	withChannelList $ manipulate $ CL.update time cname cid ctitle private (Set.toList remUsers)
	distributeChannelInformation
	-- resend announcements for public channels that we have merged:
	case oldChannelInfo of
		Just (ocid, _, ousers) -> when (not private && (ocid /= cid)) $ do
			-- query merged channel information
			newChannelInfo <- withChannelList $ query $ CL.lookupPublic cname
			case newChannelInfo of
				Just (ncid, nctitle, nusers) -> mergeChannel cname ocid ncid nctitle nusers
				Nothing -> return ()
		Nothing -> return ()

sendAnonymousMessage :: TTL -> String -> [ Attachment ] -> UTCTime -> ServerMonad ()
sendAnonymousMessage ttl text attachments time = do
	rl <- randomLocal
	case rl of
		Nothing -> btrace $ "Can't send anonymous message as we do not have local users (note that this is because of the fucked up protocol)"
		Just (sender, privkey) -> do
			msgID <- newMessageId
			sendMessageBroadcast (Flood (Routed ttl sender msgID (Anonymous text attachments time 0) (Left privkey)))

sendMsgToChannel :: UserID -> PrivateKey -> ChannelName -> ChannelID -> [ UserID ] -> String -> [ Attachment ] -> ServerMonad ()
sendMsgToChannel sender privKey cname cid receivers text attachments = do
	time <- curTime
	msgID <- newMessageId
	-- check whether we have to encrypt the content or not
	isPrivate <- withChannelList $ query $ CL.isPrivate cname cid
	-- we may need the unencrypted message in both cases, so we define it here:
	let unencMsg = Message receivers cname cid (UnencryptedMessage text attachments) time 0
	case isPrivate of
		Just True -> do
			key <- withChannelList $ query $ CL.getKey sender cname cid
			case key of
				Just rkey -> do
					-- create random initial vector
					iv <- withRandom (\gen -> let (res,ngen) = randomR (1,2^64 - 1) gen in (ngen,res))
					-- encrypt the message
					let content = EncryptedMessage (encrypt rkey (fromInteger iv) text) (map (encryptAttachment rkey (fromInteger iv)) attachments)
					sendRouted (Routed 360 sender msgID (Message receivers cname cid content time 0) (Left privKey))
				Nothing -> withPendingKey $ manipulate $ PKey.insert cname cid (Left (sender, privKey, unencMsg))
		Just False -> sendRouted (Routed 360 sender msgID unencMsg (Left privKey))
		Nothing -> return ()

requestCertificate :: UserID -> UserID -> ServerMonad ()
requestCertificate sender for = do
	btrace $ "Requesting certificate for "++(show for)++" from local user "++(show sender)
	msgID <- newMessageId
	-- send GetCertificate request
	sendRouted (Routed 360 sender msgID (GetCertificate for) (Right Nothing))

requestCertificates :: UserID -> Set.Set UserID -> Map UserID Certificate -> CertificateCallback -> ServerMonad ()
requestCertificates sender users certs cb = do
	-- add request to the certificate request list
	withCertificateRequests $ manipulate $ (++[ (users, certs, cb) ])
	-- request certificates for users we do not have yet one
	mapM_ (requestCertificate sender) (Set.toList users)

processCertificateRequest :: UserID -> Certificate -> CertificateRequest -> CertificateRequest
processCertificateRequest for cert (users, usersWithCert, cb)
	= (users', usersWithCert', cb)
	where
		usersWithCert' = if (Set.member for users) then Map.insert for cert usersWithCert else usersWithCert
		users'         = Set.delete for users

checkCertificateRequests' :: [ CertificateRequest ] -> ([ CertificateRequest ], [ ServerMonad () ])
checkCertificateRequests' requests = (snd requests', actions)
	where
		requests' = List.partition (\(users, _, _) -> Set.null users) requests
		actions   = map (\(_, certs, cb) -> (cb certs)) (fst requests')

checkCertificateRequests :: UserID -> Certificate -> ServerMonad ()
checkCertificateRequests for cert = do
	-- fullfil all requests that need the given cert
	withCertificateRequests $ manipulate $ (map (processCertificateRequest for cert))
	-- check whether some requests are no longer waiting for any certificates
	actions <- withCertificateRequests $ checkCertificateRequests'
	-- call their callbacks
	mapM_ id actions

processPendingKeyMessage :: Word64 -> PendingKeyMessage -> ServerMonad ()
processPendingKeyMessage key msg = do
	btrace $ "Processing pending key message ..."
	case msg of
		Left (sender, privKey, tc@(Message _ _ _ (UnencryptedMessage text a) _ _)) -> do
			btrace $ "Is local message to be sent ..."
			time <- curTime
			msgID <- newMessageId
			-- create random initial vector
			iv <- withRandom (\gen -> let (res,ngen) = randomR (1,2^64 - 1) gen in (ngen,res))
			sendRouted (Routed 360 sender msgID (tc { messageContent = EncryptedMessage (encrypt key (fromInteger iv) text) (map (encryptAttachment key (fromInteger iv)) a) }) (Left privKey))
		Right (sender, (Message receivers cname cid (EncryptedMessage text attachments) _ _), time, delayed) -> do
			btrace $ "Is foreign message to be decrypted ..."
			case decryptMessageContent key text attachments of
				Nothing -> berror $ "Failed to decrypt message from "++show sender
				Just (decText, decAttachments) -> mapM_ ((flip sendUser) (Receive cname cid (Just sender) decText decAttachments time delayed)) receivers

obscureMessage' :: RandomGen g => g -> ObscureMessage -> (UserID, Certificate) -> ObscureMessage
obscureMessage' gen msg (user, cert) = Right (user, encryptedMsg)
	where
		nestedMsg    = case msg of
		                   Left (text, attachments, time)
		                       -> generateMessage' $ genRootElem "chat-message" (genAnonymous False Nothing Nothing text attachments time) True Nothing 360 (Right Nothing)
		                   Right (receiver, RSAEncrypted content)
		                       -> generateMessage' $ genRootElem "chat-message" (genObscure receiver Nothing content) False Nothing 32 (Right Nothing)
		encryptedMsg = rsaEncrypt gen cert nestedMsg

obscureMessage :: PendingAnonymousMessage -> [ (UserID, Certificate) ] -> ServerMonad (ObscureMessage)
obscureMessage (PendingAnonymousMessage ttl _ text attachments time) users = do
	btrace $ "Creating obscure messages ..."
	rand <- newRandom
	let (_, obscMsg) = foldl (\(rand', msg) usr -> let (g1, g2) = System.Random.split rand' in (g2, obscureMessage' g1 msg usr)) (rand, Left (text, attachments, time)) users
	return (obscMsg)

sendObscure :: Routed (RSAEncrypted String) () -> ServerMonad ()
sendObscure obscMsg@(Routed ttl receiver msgID obscData _) = do
	btrace $ "Got an obscure for "++(show receiver)++" ..."
	-- route obscure message:
	localUsers <- withLocalUserInfo $ query $ users
	if Set.member receiver localUsers
		then do
			btrace $ "Obscure for local user, processing ..."
			processObscure ttl receiver msgID obscData
			btrace $ "Obscure processed."
		else do
			btrace $ "Obscure for foreign user, forwarding ..."
			-- query routing table
			table <- withRoutingTable $ query $ id
			-- lookup routes,
			-- we don't care about errors here, if message could not be routed, we or the sender
			-- will try it later (ensured through pending structure)
			let (addrs, _) = route table obscMsg
			mapM_ (\(addr, msg) -> sendMessage addr (Obscure msg)) (Map.assocs addrs)
			btrace $ "Obscure forwarded."

sendAnonymousCallback :: PendingAnonymousMessage -> CertificateCallback
sendAnonymousCallback paMsg@(PendingAnonymousMessage ttl _ text attachments mtime) users = do
	-- do obscuring here ...
	btrace $ "Obscuring message ..."
	localUsers <- withLocalUserInfo $ query $ LocInf.users
	let (lu, nlu) = List.partition (\(u, _) -> Set.member u localUsers) (Map.assocs users)
	-- lu and nlu are still sorted
	lu'  <- withRandom (\gen -> shuffle gen lu)  -- shuffle local users
	nlu' <- withRandom (\gen -> shuffle gen nlu) -- shuffle remote users
	let (start, nlu'') = if List.null nlu'
		then ([],          [])
		else ([head nlu'], tail nlu')

	restPath <- withRandom (\gen -> shuffle gen (lu' ++ nlu''))
	let path = restPath ++ start
	btrace $ "Chosen path: " ++ (show $ reverse $ path)
	obscMsg <- obscureMessage paMsg path
	case obscMsg of
		Right (receiver, content) -> do
			msgID <- newMessageId
			-- send the obscure message
			sendObscure (Routed ttl receiver msgID content ())
			-- add it to our pending structure so we can observe whether it will be posted or not
			time <- curTime
			withPendingAnonymous $ manipulate $ PAnon.insert time paMsg
		Left (_, _, _) -> return ()

sendAnonymous :: PendingAnonymousMessage -> ServerMonad ()
sendAnonymous paMsg@(PendingAnonymousMessage ttl sender text attachments time) = do
	users <- randomUsers 5
	if (length users) > 2
		then do
			(usersWOCert, usersWCert) <- seperateUsersWithCert users
			if (Set.null usersWOCert) 
				then do
					btrace $ "Having all certificates to obscure message so I'm just doing it ..."
					sendAnonymousCallback paMsg usersWCert
				else do
					btrace $ "Not having all certificates, starting requests ..."
					-- query one local user for certificate requests
					rl <- randomLocal
					case rl of
						Nothing -> berror $ "(sendAnonymous): This should not happen"
						Just (sender, _) -> requestCertificates sender usersWOCert usersWCert (sendAnonymousCallback paMsg)
		else sendUser sender (ErrGeneral MessageWarning "Message not delivered"
			$ "Sorry, I'm omitting your message. I'm unable to send it anonymously, because there are not enough users on the network.\n\n"
			++ "I need at least three users to obscure the origin of a message.")

giveAck :: SockAddr -> UserID -> MessageID -> ServerMonad ()
giveAck to for msgid = sendMessage to (Ack for msgid)

giveNack :: Routed TargetContent InternalSignature -> ServerMonad ()
giveNack (Routed _ _ _ (Nack _) _) = return () -- Don't give Nacks to Nacks
giveNack (Routed _ sender _ (Message receivers cname cid (EncryptedMessage enc attachments) time _) (Left key)) = do
	key <- withChannelList $ query $ CL.getKey sender cname cid
	case key of
		Just rkey -> case decryptMessageContent rkey enc attachments of
			Just (rtext,rattachments) -> sendUser sender (ErrNotDelivered receivers cname cid rtext rattachments time)
			Nothing -> berror $ "Failed to decrypt message from "++show sender
		Nothing -> return () -- This case should not happen
giveNack (Routed _ sender _ (Message receivers cname cid (UnencryptedMessage text attachments) time _) (Left key)) = sendUser sender (ErrNotDelivered receivers cname cid text attachments time)
giveNack (Routed _ sender _ tc (Left key)) = do
--	is this really of interest for the user? undelivered chatmessages
--	are reported elsewhere; besides, the message won't help the user at all
--	sendUser sender (ErrGeneral "Some message could not be routed")
	btrace $ "Some message could not be routed:"
	btrace $ show tc
giveNack rt@(Routed _ _ _ _ (Right sig)) = do
	rl <- randomLocal
	case rl of
		Nothing -> btrace $ "Can't give NACK because there's no local user (blame the stupid protocol!)"
		Just (sender,privkey) -> do
			msgid <- newMessageId
			sendRouted (Routed 360 sender msgid (Nack (rt {routedSignature = sig})) (Left privkey))

-- ----------------------------
-- - SENDING HELPER FUNCTIONS -
-- ----------------------------

resend :: SockAddr -> Routed TargetContent InternalSignature -> Set.Set SockAddr -> UTCTime -> ServerMonad ()
resend addr rt tried received = do
	now <- curTime
	withPendingAck $ manipulate $ PAck.reinsert received now rt addr tried
	sendMessage addr (Target rt)

sendRoutedWithStrategy :: RoutingStrategy rs => rs -> Routed TargetContent InternalSignature -> ServerMonad ()
sendRoutedWithStrategy strat routed = do
	let (addrs,noroute) = route strat routed
	mapM_ (\(addr,tc) -> sendMessage addr (Target tc)) (Map.toList addrs)
	time <- curTime
	withPendingAck $ manipulate $ flip (foldl (\pa (addr,rt) -> PAck.insert time rt addr pa)) (Map.assocs addrs)
	case noroute of
		Nothing -> return () -- Each message could be routed
		Just nackcont -> giveNack nackcont

sendRouted :: Routed TargetContent InternalSignature -> ServerMonad ()
sendRouted routed = do
	table <- withRoutingTable $ query id
	sendRoutedWithStrategy table routed

-- --------------------------
-- - OTHER HELPER FUNCTIONS -
-- --------------------------

randomLocal :: ServerMonad (Maybe (UserID,GnuTLS.PrivateKey))
randomLocal = withLocalUserInfo (query (fmap (\(user,(chans,key,handle)) -> (user,key)).listToMaybe.(Map.assocs)))

randomUsers :: Int -> ServerMonad ([ UserID ])
randomUsers n = do
	-- query all known users from routing table and local user info
	users <- withRoutingTable $ query $ RT.userList
	localUsers <- withLocalUserInfo $ query $ LocInf.users
	-- shuffle users and return n of them
	withRandom (\gen -> shuffle gen (Set.toList (Set.union users localUsers))) >>= return.(take n)

-- | Takes a list of 'UserID's and splits them into a set lacking a certificate in "CertificateList" and a map of 'UserID's and 'Certificate's
seperateUsersWithCert :: [ UserID ] -> ServerMonad (Set.Set UserID, Map UserID Certificate)
seperateUsersWithCert users = do
	usersWithCert <- withCertificates $ query $ Map.filterWithKey (\user _ -> user `elem` users)
	let usersWOCert = users \\ (Map.keys usersWithCert)
	return (Set.fromList usersWOCert, usersWithCert)

distributeChannelInformation :: ServerMonad ()
distributeChannelInformation = do
	mp <- withChannelList $ query $ CL.channelMap
	users <- withRoutingTable $ query $ userList
	localUsers <- withLocalUserInfo $ query $ LocInf.users
	let rmp = Map.insert (mkChannelName "Anonymous", ChannelID "Anonymous" "Anonymous") ("Anonymous", False, Set.union users localUsers) mp
	sendUserBroadcast (AllChans rmp)

shuffle :: RandomGen g => g -> [a] -> (g, [a])
shuffle gen list = shuffle' gen (length list) list

shuffle' :: RandomGen g => g -> Int -> [a] -> (g, [a])
shuffle' gen _ []  = (gen, [])
shuffle' gen len list = let
	(index, gen')   = randomR (0, len - 1) gen
	(left, r:right) = splitAt index list
	(gen'', rest)   = shuffle' gen' (len - 1) (left ++ right)
	in (gen'', r:rest)

-- | An Array of (randomly chosen) hellos
hellos :: Array Int String
hellos = listArray (1, 10)
	[ "Deine Muddi!"  -- it's classical...
	, "Haskell rules" -- :)
	, "Hi, this is Barracuda" -- standard
	, "Greetings from Barracuda"
	, "Debugging sucks..."
	, "Barracuda > Palaver ;)"  -- being offensive is always a good thing ;)
	, "Barracuda > AdBee ;)"
	, "Barracuda > MAdchat ;)"
	, "May the force be with you (if you're using Barracuda)"
	, "ping"
	]

