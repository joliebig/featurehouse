-- |
-- Maintainer: Stephan Friedrichs
--
-- The main module to spawn the entire application.
module Main (
	main
) where

import Barracuda.Distributor
import Barracuda.GUI
import Barracuda.GUI.ServerInterface
import Barracuda.GUI.Infrastructure
import Barracuda.ServerState
import Barracuda.Utils
import Control.Concurrent
import Control.Concurrent.STM -- Simon rules :)
import Control.Monad
import qualified Data.ByteString as BS
import Data.Char (isDigit)
import Data.Char.UTF8 (fromUTF8)
import Data.IORef
import qualified Data.List as List
import Data.Set hiding (null, map)
import Data.Time
import Graphics.UI.Gtk hiding (Socket)
import Network.AdHoc.Generator
import Network.AdHoc.Message
import Network.AdHoc.UserID
import qualified Network.GnuTLS as GnuTLS
import Network.Socket (withSocketsDo)
import Network.Socket
import System.Environment
import System.IO
import System.Random
import System.Console.GetOpt
import System.Exit
import Text.ParserCombinators.Parsec(parse)

-- | The options of a Barracuda instance.
data Options = Options
	{ userNumber   :: Int
	, portNumber   :: PortNumber
	, infras       :: Maybe (Set SockAddr)
	, bcastAddress :: HostAddress
	, showHelp     :: Bool
	, beVerbose    :: Bool
	, debug        :: Bool
	} deriving (Show, Eq, Ord)

-- | A type with one constructor for each option of Barracuda.
data Option
	= OptUserNumber Int
	| OptPortNumber PortNumber
	| OptInfrastructure SockAddr
	| OptBCast HostAddress
	| OptVerbose
	| OptHelp
	| OptDebug
	deriving (Show, Eq, Ord)

-- | The default settings.
defaultOptions :: Options
defaultOptions = Options
	{ userNumber = 1
	, portNumber = fromIntegral 8888
	, bcastAddress = maxBound -- 255.255.255.255
	, infras     = Nothing
	, showHelp   = False
	, beVerbose  = False
	, debug      = False
	}

-- | Initiates a new Barracuda server including gui(s) and an (optional) infrafilter.
--   The network connections are set up, all the necessary threads are spawned and
--   the Gtks 'usafeInitGUIForThreadedRTS'- and 'mainGUI' functions are called. For
--   further description, run @Barracuda --help@ ;)
main :: IO ()
main = withSocketsDo $ GnuTLS.withGnuTLS $  do
	progname <- getProgName
	args <- getArgs
	case processArgs args of
		Left err -> putStrLn err >> putStrLn (usage progname) >>  exitWith (ExitFailure (-1))
		Right (opts, cert) -> if showHelp opts
			then putStrLn (usage progname)
			else runBarracuda opts cert

-- | Actually spawns the Barracuda server, taking evaluated 'Options'.
runBarracuda :: Options -> String -> IO ()
runBarracuda opts certPath = do
	-- certificate
	str_cert <- BS.readFile certPath
	let Right cert = GnuTLS.importCertificate str_cert GnuTLS.X509FmtPem
	-- socket
	sock <- socket AF_INET Datagram 0
	bindSocket sock (SockAddrInet (portNumber opts) iNADDR_ANY)
	setSocketOption sock Broadcast 1
	setSocketOption sock DontRoute 0
	--setSocketOption sock TimeToLive 1
	-- DistributorChan, set infrastructural data immediately
	chan <- newChan
	getCurrentTime >>= \now -> writeChan chan (now, InfraMsg (infras opts))
	-- fork timer, socket and guis
	forkIO $ mainPort sock chan
	forkIO $ timer chan
	killGui <- forkGUIs chan (debug opts) (infras opts) (userNumber opts)
	-- finally run a ServerMonad with the DistributorChans contents
	msgs <- getChanContents chan
	runServerMonad
		(\ids addr msg -> let (xml, nids) = generateMessage ids msg in do
			if length xml < 65536
				then do
--					putStrLn $ "-----------outgoing-----------\n" ++ xml ++ "\n------------------------------"
					sendTo sock xml addr
					return nids
				else return ids)
		(beVerbose opts) (bcastAddress opts) cert (portNumber opts)
		(map (\(time, msg) -> (time, processMessage msg)) msgs)
	killGui

-- | A timer that is to be started as thread. It pings the 'DistributorChan' in a
--   random interval between 1.5 and 2.5 seconds (i.e. the HELLO interval).
timer :: DistributorChan -> IO ()
timer chan = do
	delay <- randomRIO (1500000, 2500000)
	threadDelay delay
	time <- getCurrentTime
	writeChan chan (time, TimeMsg)
	timer chan

-- | Listens at the given 'Socket' and filters everything received from the
--   own server.
mainPort :: Socket         -- ^ An initialized 'Socket' for UDP communication.
	-> DistributorChan -- ^ Will receive all incomming messages.
	-> IO ()
mainPort sock chan = do
	(str_msg, rlen, from) <- recvFrom sock 65535
	let str_msg_enc = maybe str_msg id $ fromUTF8 str_msg
--	putStrLn $ "-----------incoming-----------\n" ++ str_msg ++ "\n------------------------------"
	now <- getCurrentTime
	writeChan chan (now, ProtMsg from str_msg_enc)
	mainPort sock chan

-- | Initiates GUIs and an optional InfraGUI.
forkGUIs :: DistributorChan     -- ^ Will receive incomming GUI or InfraGUI messages.
	-> Bool                 -- ^ True if and only if an InfraGUI shall pop up.
	-> Maybe (Set SockAddr) -- ^ Initial configuration for the InfraGUI (see there).
	-> Int                  -- ^ How many GUIs to spawn. Zero is OK, negatives are treated alike.
	-> IO (IO ())
forkGUIs chan infragui infradata n = do
	forkOS $ do
		unsafeInitGUIForThreadedRTS
		if infragui 
			then spawnInfra (\infra -> do
				time <- getCurrentTime
				writeChan chan (time, InfraMsg infra)
				) infradata
			else return ()
		replicateM_ n $ newGUI chan guiNew
		mainGUI
	return (postGUIAsync mainQuit)

-- | Spawns an infrastructure gui.
spawnInfra :: InfraGUI
spawnInfra send _ = do -- TODO: do not ignore initial configuration
	win <- windowNew
	inf <- infrastructureWidgetNew
	win `containerAdd` (infrastructureWidgetGet inf)
	inf `onUpdate` (\lst -> send (fmap fromList lst))
	widgetShowAll win

-- | Spawns a new 'GUI'.
newGUI :: DistributorChan -- ^ Will receive messages from the gui.
	-> GUI            -- ^ Function used to spawn the gui.
	-> IO ()
newGUI chan spawnGUI = mdo
	(registered, userRef) <- atomically $ do
		r <- newTVar False
		u <- newTVar Nothing
		return (r, u)
	respondFunction <- spawnGUI (\message -> do
		time <- getCurrentTime
		case message of
			SetUser uid cert pk -> do
				atomically $ writeTVar userRef $ Just (uid, cert, pk)
				writeChan chan (time,NewGUI uid cert pk respondFunction)
			_                   -> do
				(reg, info) <- atomically $ do
					r <- readTVar registered
					i <- readTVar userRef
					return (r, i)
				case info of
					Nothing          -> return ()
					Just (uid, cert, pk) -> do
						if not reg
							then do
								writeChan chan (time, NewGUI uid cert pk respondFunction)
								atomically $ writeTVar registered True
							else return ()
						writeChan chan (time, (GUIMsg uid message))
		)
	return ()

-- | Parses the command-line arguments. The IO monad fails to indicate invalid
--   arguments. If the IO monad returns 'Nothing', the help option has been
--   supplied. Otherwise 'Just' 'Options' is returned (in the IO monad).
processArgs :: [String] -> Either String (Options, String)
processArgs args = let (opts, nargs, errs) = getOpt Permute options args
	in if null errs
		then (case nargs of
			[cert] -> Right (foldl foldOptions defaultOptions opts, cert)
			[]     -> Left "no certificate given"
			_      -> Left "too many certificates given")
		else Left (unlines errs)

-- | Applies one 'Option' to a an 'Options' bundle.
foldOptions :: Options -> Option -> Options
foldOptions opts (OptUserNumber num) = opts {userNumber = num}
foldOptions opts (OptPortNumber num) = opts {portNumber = num}
foldOptions opts (OptInfrastructure addr) = opts {infras = Just $ maybe (singleton addr) (insert addr) (infras opts)}
foldOptions opts (OptBCast addr) = opts {bcastAddress = addr}
foldOptions opts OptHelp = opts {showHelp = True}
foldOptions opts OptVerbose = opts {beVerbose = True}
foldOptions opts OptDebug = opts {debug = True}

-- | All 'Option's for Barracuda
options :: [OptDescr Option]
options =
	[ Option ['b'] ["broadcast"] (ReqArg ((OptBCast).readHost') "<ip>") "use <ip> as the broadcast address (default is 255.255.255.255)"
	, Option ['p'] ["port"] (ReqArg ((OptPortNumber).fromIntegral.read) "<n>") "use specified network port <n> (default is 8888)"
	, Option ['u'] ["users"] (ReqArg ((OptUserNumber).read) "<n>") "spawn <n> user inerfaces (default is 1)"
	, Option ['d'] ["debug"] (NoArg OptDebug) "activate the debugging- (infrastructure-) mode (default is off)"
	, Option ['i'] ["infrastructure"] (ReqArg ((OptInfrastructure).(readSockAddr' 8888)) "<addr:[p]>") "add address and port to infrastructure filter and activate it, but do not spawn the infrastucture gui (see -d)"
	, Option ['v'] ["verbose"] (NoArg OptVerbose) "verbose (default is off)"
	, Option ['h'] ["help"] (NoArg OptHelp) "displays this text"
	]

-- | The usage message displayed, when the --help option is supplied.
usage :: String   -- ^ The name of the called binary.
	-> String
usage name = usageInfo ("Usage:\n" ++ name ++ " [-b <n>] [-p <n>] [-u <n>] [-d] [-i <addr:[p]>] [-v] [-h] certificate") options
