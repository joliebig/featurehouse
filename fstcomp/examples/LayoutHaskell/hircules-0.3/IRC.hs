--  IRC.hs: an Internet Relay Chat library
--
--  Version: $Revision: 1.1 $ from $Date: 2008-10-14 18:01:24 $
--
--  Copyright (c) 2003 Andrew J. Bromage
--  Copyright (c) 2003 Jens-Ulrik Holger Petersen
--
--  See the "COPYING" file for license information.

module IRC (IRC,
            IRCMessage(..),
            IRCRWState,
            addChannel,
            checkPrivs,
            displayIRCchannel,
            getIRCChannel,
            getIRCChannels,
            getNick,
            getUserChannels,
            ircCommands,
            ircChannels,
            ircDisplay,
            ircDisplayAlert,
            ircDisplayAll,
            ircInput,
            ircInstallModule,
            ircJoin,
            ircModules,
            ircModuleState,
            ircPart,
            ircPrivmsg,
            ircRead,
            ircReadChan,
            ircReady,
            {-ircSend,-}
            ircSignOn,
            ircTopic,
            ircQuit,
            ircWrite,
            ircWriteEnc,
            {-deprecated-}ircnick,
            joinChanUser,
            logMessage,
            msgNick,
            msgUser,
            {-memberChannel,-}
            mkIrcMessage,
            mkIrcMessageWithPrefix,
            partChanUser,
            removeChannel,
            removeUser,
            renameUser,
            runIRC,
            setChanUsers,
            setChannelCoding,
            setNick,
            stripMS,
            Module(..),
            ModuleState(..),
            MODULE(..))
where

import Network (connectTo, withSocketsDo, PortID(..))
import Monad
import Maybe
import GHC.IO
import GHC.IOBase (Handle, BufferMode(..))
import GHC.Handle
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (toLower)
import Data.List
import Data.FiniteMap
import Data.Dynamic
import Data.IORef
import System.Exit
import System.IO (hPutStrLn)
-- import System.IO.Unsafe (unsafePerformIO)
import System.Locale
import System.Time

-- import Gtk (labelSetText, widgetShow)

import Channel
import Charset
import Config (logDir)
import Debug
import Directories ((+/+))
import EntryArea (setNickText)
import GUI
import MaybeDo
import Threads
import WordString

data IRCRState
  = IRCRState { ircServer      :: String
              , ircReadChan    :: Chan IRCMessage
              , ircReadThread  :: ThreadId
              , ircWriteChan   :: Chan IRCMessage
              , ircWriteThread :: ThreadId
              ,	ircSocket      :: Handle
              ,	ircLogFile     :: Handle
              }

data IRCRWState
  = IRCRWState { ircPrivilegedUsers :: FiniteMap String Bool
	       , ircChannels        :: FiniteMap String IRCChannel -- name chan
	       , ircNick            :: String
	       , ircModules         :: FiniteMap String MODULE
	       , ircCommands        :: FiniteMap String MODULE
	       , ircModuleState     :: FiniteMap String (IORef ModuleState)
	       , ircUsers           :: FiniteMap String [String] -- nick chans
	       }

data IRCMessage
  = IRCMessage {
        msgPrefix   :: String,
        msgCommand  :: String,
        msgMiddle   :: String,
	msgTail     :: String
  }

instance Show IRCMessage where
    showsPrec d (IRCMessage prefix cmd mid tale) = showParen (d >= 10) $ showString showStr
       where
       showStr = (if null prefix then "" else (':':prefix) ++ " ") ++ cmd ++
		 (if null mid then "" else " " ++ mid) ++
		 (if null tale then "" else " " ++ ':':tale)

msgNick :: IRCMessage -> String
msgNick msg = fst $ break (== '!') (msgPrefix msg)

msgUser :: IRCMessage -> String
msgUser msg =
    let rest = snd $ break (== '!') (msgPrefix msg) in
    if null rest
       then ""
       else case tail rest of
                "" -> ""
                ('~':cs) -> cs
                cs -> cs

-- (deprecated) lambdabot compatibility
ircnick :: IRCMessage -> String
ircnick = msgNick

data ModuleState = forall a. (Typeable a) => ModuleState a

type IRC a = StateT IRCRWState (ReaderT IRCRState IO) a

mkIrcMessage :: String -> String -> String -> IRCMessage
mkIrcMessage cmd middle tale
  = IRCMessage { msgPrefix = "", msgCommand = cmd, msgMiddle = middle, msgTail = tale }

mkIrcMessageWithPrefix :: String -> String -> String -> String -> IRCMessage
mkIrcMessageWithPrefix prefix cmd middle tale
  = IRCMessage { msgPrefix = prefix, msgCommand = cmd, msgMiddle = middle, msgTail = tale }

getNick :: IRC String
getNick = gets ircNick

setNick :: String -> IRC ()
setNick name = do
    modify (\ s -> s { ircNick = name })
    chans <- getIRCChannels
    liftIO $ mapM_ (\ chan -> setNickText chan name) $ filter chanreal chans

ircSignOn :: String -> String -> IRC ()
ircSignOn nick name
  = do  server <- asks ircServer
        ircWrite (mkIrcMessage "USER" (nick +-+ "localhost" +-+ server) name)
        ircWrite (mkIrcMessage "NICK" nick "")

-- ircGetChannels :: IRC [String]
-- ircGetChannels
--   = do  chans <- gets ircChannels
--         return (keysFM chans)

getIRCChannel :: String -> IRC (Maybe IRCChannel)
getIRCChannel name = do
    chans <- gets ircChannels
    return $ lookupFM chans (map toLower name)

getIRCChannels :: IRC [IRCChannel]
getIRCChannels = do
    chans <- gets ircChannels
    return $ eltsFM chans

-- memberChannel :: String -> IRC Bool
-- memberChannel name = do
--     mchan <- getIRCChannel name
--     return $ isJust mchan

addChannel :: String -> Bool -> IRC ()
addChannel name real = do
    mchan <- getIRCChannel name
    when (isNothing mchan) $ do
      let lname = map toLower name
      nick <- getNick
      chan <- liftIO $ addIRCchannel lname nick real
      modify (\ s -> s { ircChannels = addToFM (ircChannels s) lname chan })
      joinChanUser nick name

removeChannel :: String -> IRC ()
removeChannel name = do
    modify (\ s -> s {ircChannels = delFromFM (ircChannels s) (map toLower name)})

ircPrivmsg :: String -> String -> IRC ()
ircPrivmsg who msg = do
    nick <- getNick
    if (who /= nick) then mapM_ ircPrivmsg' (lines msg) else return ()
  where
  ircPrivmsg' :: String -> IRC ()
  ircPrivmsg' line = ircWrite (mkIrcMessage "PRIVMSG" who line)

ircTopic :: String -> String -> IRC ()
ircTopic chan topic
  = ircWrite (mkIrcMessage "TOPIC" chan topic)

ircQuit :: IRC ()
ircQuit
  = do  ircWrite (mkIrcMessage "QUIT" "" "")
        liftIO (exitWith ExitSuccess)

ircJoin :: String -> IRC ()
ircJoin loc
  = ircWrite (mkIrcMessage "JOIN" loc "")

ircPart :: String -> IRC ()
ircPart loc
  = ircWrite (mkIrcMessage "PART" loc "")

ircReady :: Chan a -> IRC Bool
ircReady chan =
    liftIO $ liftM not $ isEmptyChan chan

ircRead :: IRC IRCMessage
ircRead = do
    chanR <- asks ircReadChan
    liftIO (readChan chanR)

ircWrite :: IRCMessage -> IRC ()
ircWrite msg = do
    s <- ask
    liftIO $ writeChan (ircWriteChan s) msg

ircWriteEnc :: Channel -> IRCMessage -> IRC ()
ircWriteEnc ch msg = do
    s <- ask
    chan <- getIRCChannel ch
    liftIO $ maybeDo chan $ \ chn -> writeChan (ircWriteChan s) (encodeMsg $ chancoding chn)
    where
    mid = msgMiddle msg
    tale = msgTail msg
    encodeMsg :: Maybe String -> IRCMessage
    encodeMsg coding = msg { msgMiddle = encodeCharset coding mid,
                             msgTail = encodeCharset coding tale}

ircInput :: IRC Interactive
ircInput = liftIO (readChan chanI)

-- ircSend :: IRCMessage -> IRC ()
-- ircSend line
--   = do  h <- asks ircSocket
--         liftIO $ hPutStrLn h $ line ++ "\r"
-- 	ircDisplay rawchannel $ "i " ++ line
-- 	logMessage line
-- 	nick <- getNick
-- 	liftIO $ writeChan (ircReadChan s) (msg {msgPrefix = nick ++ "!"})

ircDisplay :: Channel -> Bool -> String -> IRC ()
ircDisplay ch alert str = do
    chan <- getIRCChannel ch
    liftIO $ maybeDo chan (\ chn -> writeTextLn chn alert str)

ircDisplayAll :: String -> IRC ()
ircDisplayAll str = liftIO $ writeTextLn allchannel False str

ircDisplayAlert :: Bool -> String -> IRC ()
ircDisplayAlert alert str = liftIO $ writeTextLn alertchannel alert str

runIRC :: Maybe String -> (IRC ()) -> IO ()
runIRC mserver m =
    try runIrc' >>= \exc ->
        case exc of
            Left exception -> putStrLn ("Exception: " ++ show exception)
            Right result   -> return result
  where
  runIrc' :: IO ()
  runIrc' = withSocketsDo $ do
        s <- liftIO (connectTo hostname (PortNumber portnum))
        hSetBuffering s NoBuffering
        threadmain <- myThreadId
        chanR <- newChan
        chanW <- newChan
        threadr <- newThread (readerLoop threadmain chanR chanW s)
        threadw <- newThread (writerLoop threadmain chanW s)
	h <- liftIO $ openFile (logDir +/+ hostname) AppendMode
	hSetBuffering h LineBuffering
        let chans = IRCRState { ircServer      = hostname
                              , ircReadChan    = chanR
                              , ircReadThread  = threadr
                              , ircWriteChan   = chanW
                              , ircWriteThread = threadw
                              , ircSocket      = s
                              , ircLogFile     = h
			      }
        bracket_ (return ()) (killThread threadr >> killThread threadw)
		     (runReaderT (evalStateT m initState) chans)
  (hostname,port') = break (== ':') $ fromJust mserver
  port = if null port' then "6667" else tail port'
  portnum = fromIntegral (read port :: Integer)
  initChans = listToFM [("%all",allchannel),
                        ("%raw",rawchannel),
                        ("%raw",rawchannel)]
  initState =
	    IRCRWState { ircPrivilegedUsers = listToFM [ (user,True) | user <- [] ]
		       , ircChannels = initChans
		       , ircNick = ""
		       , ircModules = emptyFM
		       , ircCommands = emptyFM
		       , ircModuleState = emptyFM
		       , ircUsers = emptyFM
                       }

readerLoop :: ThreadId -> Chan IRCMessage -> Chan IRCMessage -> Handle -> IO ()
readerLoop threadmain chanR chanW h
  = do  -- liftIO (putStrLn "Running reader loop...")
        exc <- try readerLoop'
        case exc of
           Left err -> throwTo threadmain err
           Right _ -> return ()
  where
    readerLoop'
      = do eof <- hIsEOF h
	   if eof
	      then hClose h
	      else do
		   input <- hGetLine h
                   debug "input" $ init input
		   -- remove trailing '^M'
		   let line = init input  -- [ c | c <- line, c /= '\n', c /= '\r' ]
		   writeTextRaw $ "r " ++ line
		   case (whead line) of
			"PING" -> writeChan chanW $ mkIrcMessage "PONG" "" (tail $ wtail line)
			_ -> writeChan chanR $ decodeMessage line
		   readerLoop'

-- removeLeadColon :: [String] -> [String]
-- removeLeadColon [] = []
-- removeLeadColon (t:ts) = (tail t):ts

writerLoop :: ThreadId -> Chan IRCMessage -> Handle -> IO ()
writerLoop threadmain chanW h
  = do exc <- try writerLoop'
       case exc of
           Left e  -> throwTo threadmain e
           Right _ -> return ()
  where
    writerLoop'
      = do msg <- readChan chanW
-- 	   putStrLn $ show msg
	   let encmsg = encodeMessage msg
	   writeTextRaw $ "w " ++ encmsg
           hPutStrLn h $ encmsg ++ "\r"
           writerLoop'

-- interactLoop :: ThreadId -> Chan Interactive -> Chan IRCMessage -> Handle -> IO ()
-- interactLoop threadmain chanI chanW h
--   = do exc <- try interactLoop'
--        case exc of
--            Left e  -> throwTo threadmain e
--            Right _ -> return ()
--   where
--     interactLoop'
--       = do (text, chan) <- readChan chanI
-- 	   let msg = mkIrcMessage target middle tail
-- 	       encmsg = encodeMessage msg
-- 	   writeTextRaw $ "i " ++ encmsg
--            hPutStr h encmsg
--            interactLoop'

encodeMessage :: IRCMessage -> String
encodeMessage msg =
    (encodePrefix (msgPrefix msg) 
     . encodeCommand (msgCommand msg)
     . encodeParams (msgMiddle msg)
     . encodeTail (msgTail msg)) ""
  where
    encodePrefix [] = id
    encodePrefix prefix = showChar ':' . showString prefix . showChar ' '
    encodeCommand cmd = showString cmd
    encodeParams [] = id
    encodeParams p = showChar ' ' . showString p
    encodeTail [] = id
    encodeTail t = showString " :" . showString t

decodeMessage :: String -> IRCMessage
decodeMessage rawtxt =
    let txt = decodeCharset rawtxt
        first = whead txt
	(prefix,cmdparams) = if startColon first
			        then (tail first, wtail txt)
			        else ("",txt)
	(cmd, params) = (whead cmdparams, wtail cmdparams)
	(middle,tale) = breakString ":" params in
    IRCMessage { msgPrefix = prefix, msgCommand = cmd, msgMiddle = middle, msgTail = tale }

-- lowQuote :: String -> String
-- lowQuote [] = []
-- lowQuote ('\0':cs)   = '\020':'0'    : lowQuote cs
-- lowQuote ('\n':cs)   = '\020':'n'    : lowQuote cs
-- lowQuote ('\r':cs)   = '\020':'r'    : lowQuote cs
-- lowQuote ('\020':cs) = '\020':'\020' : lowQuote cs
-- lowQuote (c:cs)      = c : lowQuote cs
-- 
-- lowDequote :: String -> String
-- lowDequote [] = []
-- lowDequote ('\020':'0'   :cs) = '\0'   : lowDequote cs
-- lowDequote ('\020':'n'   :cs) = '\n'   : lowDequote cs
-- lowDequote ('\020':'r'   :cs) = '\r'   : lowDequote cs
-- lowDequote ('\020':'\020':cs) = '\020' : lowDequote cs
-- lowDequote ('\020'       :cs) = lowDequote cs
-- lowDequote (c:cs)             = c : lowDequote cs

-- ctcpQuote :: String -> String
-- ctcpQuote [] = []
-- ctcpQuote ('\001':cs) = '\134':'a'    : ctcpQuote cs
-- ctcpQuote ('\134':cs) = '\134':'\134' : ctcpQuote cs
-- ctcpQuote (c:cs)      = c : ctcpQuote cs
-- 
-- ctcpDequote :: String -> String
-- ctcpDequote [] = []
-- ctcpDequote ('\134':'a'   :cs) = '\001' : ctcpDequote cs
-- ctcpDequote ('\134':'\134':cs) = '\134' : ctcpDequote cs
-- ctcpDequote ('\134':cs)        = ctcpDequote cs
-- ctcpDequote (c:cs)             = c : ctcpDequote cs

class Module m where
  moduleName     :: m -> IRC String
  moduleSticky   :: m -> Bool
  commands       :: m -> IRC [String]
  moduleInit     :: m -> IRC ()
  process        :: m -> IRCMessage -> String -> String -> String -> IRC () -- msg target cmd rest
  moduleInit _ = return () -- by default, there's no initialisation

data MODULE = forall m. (Module m) => MODULE m

ircInstallModule :: (Module m) => m -> IRC ()
ircInstallModule modl
  = do  s <- get
        modname <- moduleName modl
        let modmap = ircModules s
        put (s { ircModules = addToFM modmap modname (MODULE modl) })
        ircLoadModule modname

ircLoadModule :: String -> IRC ()
ircLoadModule modname = do
    maybemod   <- gets (\s -> lookupFM (ircModules s) modname)
    maybeDo maybemod $ \ (MODULE m) -> ircLoadModule' m >> moduleInit m
  where
    ircLoadModule' m
      = do  cmds <- commands m
            s <- get
            let cmdmap = ircCommands s        -- :: FiniteMap String MODULE
            put (s { ircCommands = addListToFM cmdmap [ (cmd,(MODULE m)) | cmd <- cmds ] })

-- ircUnloadModule :: String -> IRC ()
-- ircUnloadModule modname
--     = do maybemod <- gets (\s -> lookupFM (ircModules s) modname)
--          case maybemod of
--                        Just (MODULE m) | moduleSticky m -> ircUnloadModule' m
--                        _ -> return ()
--     where
--     ircUnloadModule' m
--         = do modname <- moduleName m
--              cmds    <- commands m
--              s <- get
--              let modmap = ircModules s        -- :: FiniteMap String MODULE,
--                  cmdmap = ircCommands s        -- :: FiniteMap String MODULE
--                  in
--                  put (s { ircCommands = delListFromFM cmdmap cmds })

-- for lambdabot
checkPrivs :: IRCMessage -> String -> IRC () -> IRC ()
checkPrivs msg target f = do 
                          maybepriv <- gets (\s -> lookupFM (ircPrivilegedUsers s) (msgNick msg))
                          case maybepriv of
                                         Just _  -> f
                                         Nothing -> ircPrivmsg target "not enough privileges"

stripMS :: Typeable a => ModuleState -> a
stripMS (ModuleState x) = fromJust . fromDynamic . toDyn $ x

logMessage :: String -> IRC ()
logMessage txt = do
   h <- asks ircLogFile
   time <- liftIO timeStamp
   date <- liftIO dateStamp
   liftIO $ hPutStrLn h $ date +-+ time +-+ txt

dateStamp :: IO String
dateStamp = do
  ct <- (getClockTime >>= toCalendarTime)
  return $ formatCalendarTime defaultTimeLocale (iso8601DateFormat Nothing) ct

setChanUsers :: [String] -> String -> IRC ()
setChanUsers users ch = do
    mchan <- getIRCChannel ch
    maybeDo mchan setChanUsers'
    mapM_ (addUserChan ch) users
  where
  setChanUsers' :: IRCChannel -> IRC ()
  setChanUsers' chan = do
      let chan' = chan { chanusers = users }
      modify (\ s -> s { ircChannels = addToFM (ircChannels s) ch chan' })

joinChanUser :: String -> String -> IRC ()
joinChanUser user ch = do
    mchan <- getIRCChannel ch
    maybeDo mchan addChanUser
    addUserChan ch user
  where
  addChanUser :: IRCChannel -> IRC ()
  addChanUser chan = do
      let users = chanusers chan
	  chan' = chan { chanusers = users ++ [user] }
      modify (\ s -> s { ircChannels = addToFM (ircChannels s) ch chan' })

partChanUser :: String -> String -> IRC ()
partChanUser user ch = do
    mchan <- getIRCChannel ch
    maybeDo mchan removeChanUser
    removeUserChan ch user
  where
  removeChanUser :: IRCChannel -> IRC ()
  removeChanUser chan = do
      let users = chanusers chan
	  chan' = chan { chanusers = delete user users }
      modify (\ s -> s { ircChannels = addToFM (ircChannels s) ch chan' })

-- userInChan :: String -> String -> IRC Bool
-- userInChan user ch = do
--     chans <- getUserChannels user
--     return $ elem ch chans

getUserChannels :: String -> IRC [String]
getUserChannels nick = do
    users <- gets ircUsers
    debug "users" $ fmToList users
    let chans = lookupFM users (map toLower nick)
    debug "chans" chans
    return $ fromMaybe [] chans

addUserChan :: String -> String -> IRC ()
addUserChan chan user = do
    chans <- getUserChannels user
    setUserChans user $ if elem chan chans
			   then chans
			   else chans ++ [chan]

removeUserChan :: String -> String -> IRC ()
removeUserChan chan user = do
    chans <- getUserChannels user
    setUserChans user $ delete chan chans

removeUser :: String -> IRC ()
removeUser user =
    modify (\ s -> s { ircUsers = delFromFM (ircUsers s) user })

setUserChans :: String -> [String] -> IRC ()
setUserChans user chans =
    modify (\ s -> s { ircUsers = addToFM (ircUsers s) user chans })

renameUser :: String -> String -> IRC ()
renameUser old new = do
    chans <- getUserChannels old
    debug "chans" chans
    removeUser old
    setUserChans new chans
    mapM_ (partChanUser old) chans
    mapM_ (joinChanUser new) chans

displayIRCchannel :: String -> IRC ()
displayIRCchannel ch = do
    chan <- getIRCChannel ch
    liftIO $ maybeDo chan (displayChannelTab True) 

setChannelCoding :: Channel -> Maybe String -> IRC ()
setChannelCoding ch coding = do
    chan <- getIRCChannel ch
    maybeDo chan $ \ chn -> modify (\ s -> s { ircChannels = addToFM (ircChannels s) (channame chn) (chn { chancoding = coding }) })

-- getChannelCoding :: Channel -> IRC (Maybe String)
-- getChannelCoding ch = do
--     chan <- getIRCChannel ch
--     maybe (return Nothing) (\ chn -> return $ chancoding chn) chan
