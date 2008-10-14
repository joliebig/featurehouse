{-# OPTIONS_GHC -fallow-overlapping-instances #-}
module Main(main) where

import Char
import Directory
import List hiding(or,and,any,all)
import Maybe
import System.Cmd
import System.Time
import Time
import Random

import Control.Concurrent
import Control.Exception
import Data.Array.IO
import Data.Array.MArray
import Data.IORef
import Data.Unique
import qualified Data.HashTable as Hash
import qualified System.Posix as Posix
import qualified System.Posix.IO as PosixIO
import System.IO

import Atom
import Boolean.Algebra
import Boolean.Boolean
import CacheIO
import Charset
import ConfigFile
import Control.Monad.Error
import Curses
import Doc.DocLike hiding(space)
import EIO
import ErrorLog
import Filter
import Gale.Gale
import GenUtil
import GinsuConfig
import Help
import Gale.KeyCache(numberKeys)
import KeyName
import MyLocale
import Options
import PackedString
import Prelude hiding((&&),(||),not,and,or,any,all)
import Gale.Puff
import Regex
import Screen
import SHA1
import Status
import System.Locale
import Version
import Doc.Chars(rTee,hLine,lTee)

idleThreshold = 240
pufflog = "ginsu.4.pufflog"
helpText = "Need help? Press F1 or ?"

getTmpFile = do
    pid <- Posix.getProcessID
    u <- newUnique
    return ("/tmp/ginsu.puff." ++ show pid ++ "." ++ show (hashUnique u))

data MainEvent = MainEventKey Curses.Key | MainEventPuff Puff | MainEventComposed (Maybe Puff) (IO ())
insertKeys ic s = mapM_ (\c -> writeChan ic (MainEventKey c)) (stringToKeys s)

{-# NOTINLINE main #-}
main = do
    setupLocale
    configureGinsu
    (flags, acats) <- ginsuOpts
    checkConfigIsGood

    withErrorMessage bugMsg $ do

    logname <- case envErrorLog flags of
	Just x -> return x
	Nothing -> galeFile "ginsu.errorlog"

    withErrorLog logname $ withStartEndEntrys "Ginsu" $ do

    case (envVerbose flags) of
	1 -> do
	    setLogLevel LogInfo
	    putLog LogNotice $ "Verbosity level: Info"
	n | n > 1 -> do
	    setLogLevel LogDebug
	    putLog LogNotice $ "Verbosity level: Debug"
	_ -> return ()

    cs <- configLookup "CHARSET"

    charsetSetup cs


    setErrorLogPutStr $ \h s -> do
        Status.log s
        Charset.csHPutStr h s


    galeDir <- getGaleDir
    gp <- getGaleProxy
    gid <- getGaleId
    galeDomain <- getGaleDomain
    galeAliases <- getGaleAliases

    Status.set "Ginsu.Version" Version.fullName
    Status.set "Ginsu.Errorlog" logname
    Status.setF "Ginsu.Charset" MyLocale.getCharset

    Status.setF "Gale.Config.Domain" getGaleDomain
    Status.setF "Gale.Config.Id" getGaleId
    Status.setF "Gale.Config.Proxy" (fmap simpleQuote getGaleProxy)
    Status.setF "Gale.Config.Dir" getGaleDir

    putLog LogInfo $ "GALE_DIR " ++ galeDir
    putLog LogInfo $ "GALE_DOMAIN " ++ galeDomain
    putLog LogInfo $ "GALE_ID " ++ gid
    putLog LogInfo $ "GALE_PROXY " ++ simpleQuote gp
    putLog LogInfo $ "GALE_ALIASES \n" ++
	unlines ( map  (\(l,cat) -> "  " ++ l ++ " -> " ++ show cat)  galeAliases)


    Control.Exception.bracket_ initCurses endWin $ do
    b <- configLookupBool "USE_DEFAULT_COLORS"
    when b useDefaultColors

    withCursor CursorInvisible $ do

    ic <- newChan
    b <- configLookupBool "DISABLE_SIGWINCH"

    redraw_r <- newRenderContext

    Posix.installHandler Posix.sigINT Posix.Ignore Nothing
    Posix.installHandler Posix.sigPIPE Posix.Ignore Nothing
    case (b,cursesSigWinch) of
	(False,Just s) -> Posix.installHandler s (Posix.Catch (resizeRenderContext redraw_r (writeChan ic (MainEventKey KeyResize)))) Nothing >> return ()
	_ -> return ()

    gs <- fmap (concat . map words) $ configLookupList "GALE_SUBSCRIBE"
    gps <- getGaleProxy
    let c =  if envJustArgs flags then acats else gs ++ acats
        nc = map ("_gale.notice@" ++) $ snub (map categoryCell (map catParseNew c))
    --let c =  gs ++ acats
    --    nc = map ("_gale.notice@" ++) $ snub (snds (map catParseNew c))
    withGale gps $ \gc -> do

    Status.setF "Gale.Server" (fmap fromEither $ readMVar $ connectionStatus gc )
    Status.setF "Gale.Categories"  (fmap (unwords . map catShowNew ) $ readMVar $ gCategory gc )
    Status.setFS "Gale.Keys.Cached" (numberKeys $ keyCache gc)

    galeAddCategories gc $ map catParseNew (snub $ c ++ nc)
    Category (name,domain) <- fmap catParseNew getGaleId
    galeAddCategories gc $ [Category ("_gale.rr." ++ name ,domain)]
    doRender widgetEmpty
    pl <- forkIO $ puffLoop ic gc
    gl <- forkIO $ getchLoop ic
    yo <- newIORef (0::Int)
    plog <- galeFile pufflog
    ops <- if envNoPufflog flags then return [] else
        doRender (widgetCenter $  widgetText "Reading pufflog...") >> readPuffs plog
    ps <- newMVar ((reverse $ zip [1..] (reverse ops)))
    next_r <- newIORef (length ops + 1)
    pcount_r <- newIORef 0
    unless (envNoWritePufflog flags || envNoPufflog flags) $ (forkIO $ pufflogLoop ps pcount_r 0) >> return ()

    (Just s) <- configLookup "ON_STARTUP"
    insertKeys ic s

    doRender (widgetCenter $  widgetText "Entering main loop...")
    mainLoop gc ic yo ps next_r pcount_r redraw_r
    killThread pl
    killThread gl
    ps <- liftM (map snd) $ readVal ps
    unless (envNoPufflog flags || envNoWritePufflog flags) $ doRender (widgetCenter $ widgetText "Writing pufflog...") >>  writePufflog ps

pufflogLoop ps_r pcount_r n = do
    rnd <- randomIO
    threadDelay (30000000 + (rnd `mod` 10000000))
    n' <- readVal pcount_r
    if n' /= n
        then do
	    ps <- liftM (map snd) $ readVal ps_r
	    writePufflog ps
	    pufflogLoop ps_r pcount_r n'
        else pufflogLoop ps_r pcount_r n

writePufflog ps = do
    plog <- galeFile pufflog
    plsize <- fmap (read . fromJust) $ configLookup "PUFFLOG_SIZE"
    withPrivateFiles $ writePuffs plog (if (plsize > 0) then (take plsize ps) else ps)




puffLoop ic gc = repeatM_ $  galeNextPuff gc >>= \p -> (writeChan ic $ MainEventPuff p)
getchLoop ic = repeatM_ (getCh >>= \x -> writeChan ic (MainEventKey x))


apHead f (x:xs) = f x:xs
apHead _ [] = []


expandAliases :: [Category] -> IO [Category]
expandAliases cats = do
    as <- getGaleAliases
    gd <- getGaleDomain
    let ec x@(Category (c,"")) = de $ head $ [Category (tc ++ drop (length f) c,td) | (f,Category (tc,td)) <- as, f `isPrefixOf` c] ++ [x]
	ec x = x
	de (Category (c,"")) = Category (c,gd)
	de x = x
    return $ map ec cats

{-
statusBarWidget :: SVar String -> SVar Int -> SVar Int -> Widget
statusBarWidget svm svs svpc = widgetHorizontalBox False [(NoExpand,m), (Expand, widgetEmpty), (NoExpand,s), (NoExpand,widgetText "/"), (NoExpand, pc), (NoExpand,widgetText " ") ] where
    m = newSVarWidget svm widgetText
    s = newSVarWidget svs (widgetText . show)
    pc = newSVarWidget svpc (widgetText . show)
-}

type PresenceData = [(String,(String,String))]
presenceString (_,(_,n)) = n

--presenceView :: PresenceData -> Widget
presenceView idleHash pl = dynamicWidget $ do
    itl <- Hash.toList idleHash
    (TOD ct _) <- getClockTime
    let pt = unlines $ "Online:": buildTableLL (concatMap rt (gf  plon)) ++ ["", "Offline:"] ++ buildTableLL (concatMap rt (gf ploff))
        gf ts = map (\ts -> (fst (head ts), snds ts)) (groupBy (\(a,_) (b,_) -> a == b) (sort ts))
        (ploff,plon) = partition (\p -> "out" `isPrefixOf` presenceString p ) pl
        rt (a,bs) = [(a,it a ++ concatInter ", " (snub (snds bs)))]
        it a = case lookup a itl of
            Just n | n == epoch -> "[not idle]"
            Just (TOD nct _) ->  if (ct - nct) < idleThreshold then "[not idle] " else "[" ++ showDuration (ct - nct) ++ "] "
            Nothing -> ""
    return $ widgetText pt



presenceViewUser :: String -> PresenceData -> Widget
presenceViewUser user pl = widgetText pt where
    pt = user ++ ":\n" ++ indentLines 2  (unlines $ buildTableLL ([b|(a,b) <- pl, a == user]))



dialog "" = widgetEmpty
dialog s = widgetCenter $ widgetAttr [AttrBold] $ widgetSimpleFrame wb where
    wb = widgetText $ unlines ([les] ++ map f es ++ [les])
    m  = (maximum (map length es))
    f es = es ++ replicate (m - length es) ' '
    les = replicate m ' '
    es = map (\s -> " " ++ s ++ " " ) (lines s)

generateIdInstance = do
    pid <- Posix.getProcessID
    n <- liftM Posix.nodeName Posix.getSystemID
    return $ n ++ "/" ++ show pid


puffTemplate :: IO Puff
puffTemplate = do
    idText <- generateIdInstance
    gi <- getGaleId
    t <- getClockTime
    u <- newUnique
    gn <- configLookup "GALE_NAME"
    n <- case gn of
	Just n -> return ((f_messageSender,FragmentText (packString n)):)
	Nothing -> return id
    let mid = sha1String $ show t ++ " " ++ idText ++ " " ++ show (hashUnique u)
    let sig = [Signed (Key gi [])]
    let ef = n [
	    (f_messageId, FragmentText $ packString mid),
	    (fromString "id/class",FragmentText $ packString (package ++ "/" ++ version) ),
	    (fromString "id/instance", FragmentText $ packString idText),
	    (f_idTime,FragmentTime t)]
    return emptyPuff {fragments = ef, signature = sig}


type Position =  Int
data Marks = Marks !(IOArray Char (Maybe [Filter])) !(IOArray Char (Maybe Position))

newMarks = go where
    go = do
        a1 <- newArray ('0', 'z') Nothing
        a2 <- newArray ('0', 'z') Nothing
        mapM_ (checkMark a1) ['1' .. '9']
        return (Marks a1 a2)
    checkMark ma n = do
        let name = ("MARK_" ++ [n])
        v <- configLookup name
        case v of
            Nothing -> return ()
            Just z -> case parseFilter z of
                Left err -> putLog LogError ("Could not parse mark:" <+> name <+> z <+> err) >> return ()
                Right y -> writeArray ma n (Just [y])

mark_set_filter :: Marks -> Char -> [Filter] -> IO ()
mark_set_filter (Marks arr _) c f = writeArray arr c (Just f)
mark_set_pos (Marks _ arr) c p = writeArray arr c (Just p)

mark_get_value (Marks a b) c = do
    f <- readArray a c
    p <- readArray b c
    return (f,p)

{-
mark_set_value m c (f,p) = do
    mark_set_filter m c f
    mark_set_pos m c p
-}
mark_valid :: Char -> Bool
mark_valid ch = ch >= '0' && ch <= 'z'

data GinsuState = GinsuState { gsMarks :: !Marks , gsWorkspace :: !(MVar Char) }
newGinsuState = do
    marks <- newMarks
    workspace_r <- newMVar '1'
    return GinsuState { gsMarks = marks, gsWorkspace = workspace_r }


messageBox :: RenderContext -> Widget -> String -> IO Bool
messageBox rc fw s = setRenderWidget rc (stackedWidgets [keyCatcherWidget pk (dialog s), fw]) >> return True where
    pk _ =  setRenderWidget rc fw >> return True
askBox rc fw s pk = setRenderWidget rc (stackedWidgets [keyCatcherWidget pk  (dialog s), fw]) >> return True

markBox rc fw s action = askBox rc fw  s pk where
    pk (KeyChar n) | mark_valid n  = do
        action n
        setRenderWidget rc fw
        return True
    pk key = messageBox rc fw ("Invalid mark: " ++ keysToString [key])

mainLoop gc ic yor psr next_r pcount_r rc = do

    gs <- newGinsuState

    -- mark_set_r <- newIORef (listArray (0,9) (replicate 10 []))
    hw <- helpWidget
    statusWidget <- statusWidget

    selected_r <- newMVar 0
    let puffcount_r = readVal psr >>= return . length
    filter_r <- newMVar []

    presence_r <- newMVar []

    rot13_r <- newMVar False

    editing_r <- newMVar False

    presence <- configLookupElse "GALE_PRESENCE" "in.perhaps"
    myPresence <- newJVar presence

    Status.setF "Gale.Presence" (readVal myPresence)

    idleHash <- Hash.new (==) Hash.hashString
    puffRRHash <- Hash.new (==) Hash.hashString

    presenceWidget <- widgetScroll (newSVarWidget presence_r (presenceView idleHash))

    ct <- getClockTime
    userActionTime <- newJVar ct

    keyTable <- buildKeyTable
    matchTable <- buildMatchTable

    icmain <- newChan

    let fsw = newSVarWidget filter_r $ \fs ->
	    widgetHorizontalBox False (intersperse (NoExpand, widgetText " ") $ map (\w -> (NoExpand,widgetAttr [AttrReverse] (widgetText w))) (reverse $ map showFilter fs))

    selPuff <- newCacheIO $ do
        ps <-  readVal psr
        selected <- cacheIO $ readVal selected_r
        return $ lookup selected ps



    getFilteredPuffs <- newCacheIO $ do
        (_,xs) <- cacheIOeq scrSize
        ps <- cacheIO $ readVal psr
        fs <- cacheIO $ readVal filter_r
        let f p = all (`filterAp` p) fs
        return  [(x,puffHeight p xs)| x@(_,p) <- ps, f p]


    buf_size <- newCacheIO $ cacheIO getFilteredPuffs >>= \ps -> return $ sum [x| (_,x) <- ps]
    let statusHeight = do
            fs <- readVal filter_r
            return $ if length fs == 0 then 1 else 2

    let sb = dynamicWidget $ do
        let m = widgetText helpText
        s <- fmap (widgetText . show) $ readVal selected_r
        nf <- fmap (widgetText . show) $ fmap length getFilteredPuffs
        pc <- fmap (widgetText . show) $  puffcount_r
        ws <- fmap (widgetText . show) $  readVal (gsWorkspace gs)
        st <- readVal $ connectionStatus gc
        let wt s = (NoExpand,widgetText s)
            ne w = (NoExpand,w)
        let f = case st of
                Right _ -> id
                Left s -> \w -> widgetVerticalBox False [ne w, (NoExpand,widgetAttr [AttrReverse] (widgetText $ "*** " ++ s ++ " ***"))]
        return $ f $ widgetHorizontalBox False [ne m, (Expand, widgetEmpty), wt "Workspace:", ne ws ,wt " ", ne s, wt " (", ne nf , wt "/", ne pc , wt ") " ]
    let	keyError s k = messageBox rc fw (s ++ ": " ++ keysToString [k] ++ " -- " ++ helpText) >> return ()
        setMessage m = messageBox rc fw m >> return ()
	scrollPuffs x = mapVal yor (+ x)
        autoSaveMark = do
            n <- readVal (gsWorkspace gs)
            s <- readVal selected_r
            mark_set_pos (gsMarks gs) n s
            fs <- readVal filter_r
            mark_set_filter (gsMarks gs) n fs
        setWorkspace n = do
            autoSaveMark
            writeVal (gsWorkspace gs) n
            (f,s) <- mark_get_value (gsMarks gs) n
            modifyFilter $ maybe id const f
            maybe (return ()) (writeVal selected_r) s
            select_perhaps select_next
            center_puff
	filter_thread = do
	    p <-  selPuff
	    case p of
		Just (Puff {cats = [c]}) -> modifyFilter (BoolJust (FilterCategory c):)
		Just (Puff {cats = cs}) -> modifyFilter (or (map (BoolJust . FilterCategory) cs):)
		_ -> return ()

	addPuff p = do
            let author = getAuthor p
            Category (name,domain) <- fmap catParseNew getGaleId
	    case getFragmentString p (f_noticePresence') of
		Nothing -> return ()
		Just pn -> do
		    let np@(a',(b',_)) = (author, (maybe "unknown" unpackPS (getFragmentString p (fromString "id/instance")), unpackPS pn))
		    mapVal presence_r (\xs -> (np:[x| x@(a,(b,_)) <- xs, a /= a' || b /= b']))
	    case getFragmentTime p (fromString "status.idle") of
		Nothing -> return ()
                Just pn | pn == epoch -> Hash.delete idleHash author >> getClockTime >>=  Hash.insert idleHash author
		Just pn -> do
                    mt <- Hash.lookup idleHash author
                    Hash.delete idleHash author
                    Hash.insert idleHash author $ fromJust $ max mt (Just pn)
            case getFragmentString p (fromString "answer.receipt") of
                Just n | [Category (cat,dom)] <- cats p, dom == domain, ("_gale.rr." ++ name) `isPrefixOf` cat -> do
                    mv <- Hash.lookup puffRRHash (drop (10 + length name) cat)
                    Hash.delete puffRRHash (drop (10 + length name) cat)
                    Hash.insert puffRRHash (drop (10 + length name) cat) (unpackPS n:concat (maybeToMonad mv))
                _ -> return ()


	    case getFragmentString p (f_questionReceipt) of
		Nothing -> return ()
		Just _ -> buildReciept p >>= \p -> forkIO (galeSendPuff gc p) >> return ()
	    case fmap unpackPS $ getFragmentString p f_messageBody of
		Nothing -> return ()
		Just n -> when (not $ (all isSpace n) && (all (("_gale" `isPrefixOf`) . categoryHead) (cats p))) $ do
                    mt <- Hash.lookup idleHash author
                    pn <- getClockTime
                    Hash.insert idleHash author $ fromJust $ max mt (Just pn)
		    v <- configLookupList "BEEP"
		    let f = or (catMaybes (map parseFilter v))
		    when (filterAp f p) Curses.beep
		    n <- readVal next_r
		    mapVal next_r (+1)
		    mapVal pcount_r (+1)
		    mapVal psr (\ps -> ((n,p):ps))
                    sbsize <- fmap (read . fromJust) $ configLookup "SCROLLBACK_SIZE"
		    case sbsize of
		        0 -> return ()
                        _ -> do
                            let keep = min n sbsize
                            mapVal psr (\ps -> zip [keep, (keep - 1) .. 1] (snds $ take keep ps))
                            mapVal selected_r (\s -> s - (n - keep))
                            mapVal next_r (\s -> s - (n - keep))
		    select_perhaps select_next
		    --touchRenderContext rc
	composePuff :: IO () -> [Category] -> [PackedString] -> IO ()
	composePuff done cs kwds = do
	    p <- puffTemplate
	    editPuff (p {cats = cs, fragments = fragments p ++ [ (f_messageKeyword,FragmentText k) | k <- kwds]}) ic done
	getPresenceFrags = do
	    mp <- readVal myPresence
	    (TOD ct _) <- getClockTime
	    uaT@(TOD uat _) <- readVal userActionTime
	    (it,is,notIdle) <- return $ case ct - uat of
		n | n < idleThreshold -> (epoch," (not idle)",True)
		n -> (uaT," (" ++ (showDuration n) ++ " idle)",False)
	    return ([
		    (f_noticePresence',FragmentText (packString (mp ++ is))),
		    (fromString "status.presence", FragmentText $ packString mp),
		    (fromString "status.idle", FragmentTime it)],notIdle)
	buildReciept p = do
	    np <- puffTemplate
	    r <- getFragmentString p (fromString "question.receipt")
	    gid <- getGaleId
	    let rid = maybe [] (\x -> [(fromString "receipt.id",FragmentText x)]) $ getFragmentString p (f_messageId)
	    (pf,_) <- getPresenceFrags
	    return $ np { cats = [catParseNew (unpackPS r)], fragments = fragments np ++ pf ++ rid ++ [(fromString "answer.receipt",FragmentText $ packString gid)]}
	presenceLoop = do
	    let loopIsIdle ct = do
                    ct' <- waitJVarEq userActionTime ct
                    notIdle <- sendPresence True
                    if notIdle then loopNotIdle else loopIsIdle ct'
		loopNotIdle = do
		    notIdle <- sendPresence False
		    threadDelay $ (fromIntegral idleThreshold) * 1000000
		    if notIdle then loopNotIdle else readVal userActionTime >>= loopIsIdle
	    sendPresence True
	    loopNotIdle
	sendPresence sendIfNotIdle = do
            notifyCategory <- getMyNotifyCategory
	    (pf,notIdle) <- getPresenceFrags
	    when (sendIfNotIdle || not notIdle) $ do
		p <- puffTemplate
		galeSendPuff gc $ p {cats = [notifyCategory], fragments = fragments p ++ pf}
	    return notIdle
	gonePresencePuff = do
            notifyCategory <- getMyNotifyCategory
	    p <- puffTemplate
	    let ef = [(f_noticePresence' ,FragmentText (toPackedString f_outGone)), (f_noticePresence, FragmentText (toPackedString f_outGone))]
	    return $ p {cats = [notifyCategory], fragments = fragments p ++ ef }
	modifyFilter f = do
	    mapVal filter_r f
	    select_perhaps select_next >> select_perhaps select_prev
	    center_puff
	center_puff = do
	    (ys,_) <- scrSize
	    selected <- readVal selected_r
	    ps <- liftM reverse getFilteredPuffs
	    let f t (((n,_),h):_) | n == selected = (t,t+h)
		f t ((_,h):rest) = f (t + h) rest
		f _ [] = (1000000,0)
	    let (mn,mx) = (f 0) ps
	    mapVal yor (min mn)
	    sh <- statusHeight
	    mapVal yor (max (mx - (ys - sh) ))
	is_at_end = do
	    (ys,_) <- scrSize
	    bs <- buf_size
	    sh <- statusHeight
	    fmap ((max (bs - (ys - sh)) 0) ==) $ readVal yor
        scroll_end = do
	    (ys,_) <- scrSize
	    bs <- buf_size
	    sh <- statusHeight
	    writeVal yor (max (bs - (ys - sh)) 0)
	doRedraw _ (xs,ys) = do
	    bs <- buf_size
	    sh <- statusHeight
	    mapVal yor (min (max 0 (bs - (ys - sh))))
	    selected <- readVal selected_r
	    ps <- getFilteredPuffs
	    yo <- readVal yor
	    isRot13 <- readVal rot13_r
	    let rp _ [] =  return ()
		rp y (((i,p),n):rest) = do
		    when ((0,ys) `overlaps` (y - yo, y - yo + n)) $ renderPuff p xs stdScr (y - yo) (xs,ys) (i == selected) (if isRot13 then rot13 else id)
		    rp (y + n) rest
	    attempt (rp 0 (reverse ps))
	select_perhaps a = do
	    ps <- getFilteredPuffs
	    n <- readVal selected_r
	    unless (maybe False (const True) $ lookup n (fsts ps)) a
	select_prev = do
	    ps <- liftM fsts getFilteredPuffs
	    let f sel ((n,_):_) | n < sel = n
		f sel (_:ps) = f sel ps
		f sel [] = sel
	    mapVal selected_r (\s ->(f s ps))
	select_next = do
	    ps <- liftM fsts getFilteredPuffs
	    let f sel _ ((n,_):ps) | n > sel = f sel n ps
		f sel bg ((n,_):ps) | n > sel = f sel bg ps
		f _ bg _ = bg
	    mapVal selected_r (\s ->(f s s ps))

        fw = widgetVerticalBox False [(ExpandFill, mwidget), (NoExpand, widgetAttr [AttrBold] sb), (NoExpand, fsw)]
	mwidget = widgetEmpty { render = rnd,  processKey = pk}
	rnd canvas = doRedraw (origin canvas) (bounds canvas)
	continue = return True
        keyTable' = (map (\(a,b) -> (a, perform_action b)) keyTable)
        pk x = case lookup x keyTable' of
            Nothing -> case x of
                (KeyChar n) | n `elem` "123456789" -> setWorkspace n >> continue
                key -> keyError "Invalid key" key >> continue
            Just x -> x
        withEditing f = do
            lv <- readVal editing_r
            if lv then continue else writeVal editing_r True >> f
        perform_action x = case x of
                "reconnect_to_servers" -> reloadConfigFiles >> getGaleProxy >>= \gp -> galeSetProxys gc gp >>  reconnectGaleContext gc >> continue
                "ask_quit" ->
                    let pk x = case x of
                            (KeyChar 'y') -> return False
                            (KeyChar 'Y') -> return False
                            _ -> setRenderWidget rc fw >> return True
                        in setRenderWidget rc (stackedWidgets [keyCatcherWidget pk (dialog "Really quit (y/n)?"), fw]) >> return True
                "fast_quit" -> return False
                "next_line" -> scrollPuffs 1 >> continue
                "previous_line" -> scrollPuffs (-1) >> continue
                "forward_half_page" -> scrSize >>= \(ys,_) ->  scrollPuffs (ys - 2 `div` 2) >> continue
                "backward_half_page" -> scrSize >>= \(ys,_) ->  scrollPuffs (- (ys - 2 `div` 2)) >> continue
                "next_page" -> scrSize >>= \(ys,_) -> scrollPuffs (ys - 2) >> continue
                "previous_page" -> scrSize >>= \(ys,_) -> scrollPuffs (-ys - 2) >> continue
                "edit_config_file" -> do
                    gc <- galeFile "ginsu.config"
                    e <- getEditor
                    --mySystem (e ++ " " ++ gc)
                    myRawSystem e [gc]
                    reloadConfigFiles
                    --touchRenderContext rc
                    return True
                "first_puff" -> writeVal selected_r 0 >> select_perhaps select_next >> writeVal yor 0 >> continue
                "last_puff" -> do
                    ps <- readVal psr
                    case ps of
                        ((n,_):_) -> writeVal selected_r n >> select_perhaps select_prev >> scroll_end >> continue
                        _ -> continue
                "next_puff" -> select_next >> center_puff >> continue
                "previous_puff" -> select_prev >> center_puff >> continue
                "pop_one_filter" -> modifyFilter  (drop 1) >> continue
                "pop_all_filters" -> modifyFilter (const []) >> continue
                "invert_filter" -> modifyFilter (apHead f) >> continue where
                    f (BoolNot x) = x
                    f x = not x
                "swap_filters" -> modifyFilter f >> continue where
                    f (x:y:r) = y:x:r
                    f x = x
                "filter_current_thread" -> filter_thread >> continue
                "recall_combine_mark" -> markBox rc fw "Combine which filter mark?" fn where
                    fn n = do
                        (f,_) <- mark_get_value (gsMarks gs) n
                        modifyFilter $ maybe id (++) f
                "recall_filter_mark" -> markBox rc fw "Recall which filter mark?" fn where
                    fn n = do
                        (f,_) <- mark_get_value (gsMarks gs) n
                        modifyFilter $ maybe id const f
                "set_filter_mark" -> markBox rc fw "Set which mark?" fn where
                    fn n = do
                        fs <- readVal filter_r
                        mark_set_filter (gsMarks gs) n fs
                "set_mark" ->  markBox rc fw "Set which mark?" fn where
                    fn n = do
                        s <- readVal selected_r
                        mark_set_pos (gsMarks gs) n s
                "recall_mark" ->  markBox rc fw "Recall which mark?" fn where
                    fn n = do
                        (_,s) <- mark_get_value (gsMarks gs) n
                        maybe (return ()) (writeVal selected_r) s
                        select_perhaps select_next
                        center_puff
                "filter_current_author" -> do
                    p <- readVal selPuff
                    case p of
                        Nothing -> continue
                        Just p -> case getSigner p of
                            Just a -> do
                                modifyFilter (BoolJust (FilterAuthor a):)
                                continue
                            Nothing -> setMessage "No signed puff to get author from" >> continue
                "prompt_new_filter" -> do
                    (ys,_) <- scrSize
                    v <- commandRead justGetKey stdScr (ys - 1) "Filter: " ""
                    case v >>= parseFilter >>= \x -> return (modifyFilter (x:)) of
                        Right a -> a
                        Left "" -> return ()
                        Left err -> setMessage $ "Invalid filter:" <+> err
                    continue
                "prompt_new_filter_slash" -> do
                    (ys,_) <- scrSize
                    v <- commandRead justGetKey stdScr (ys - 1) "Filter: " "/"
                    case v >>= parseFilter >>= \x -> return (modifyFilter (x:)) of
                        Right a -> a
                        Left "" -> return ()
                        Left err -> setMessage $ "Invalid filter:" <+> err
                    continue
                "prompt_new_filter_twiddle" -> do
                    (ys,_) <- scrSize
                    v <- commandRead justGetKey stdScr (ys - 1) "Filter: " "~"
                    case v >>= parseFilter >>= \x -> return (modifyFilter (x:)) of
                        Right a -> a
                        Left "" -> return ()
                        Left err -> setMessage $ "Invalid filter:" <+> err
                    continue
                "toggle_rot13" -> mapVal rot13_r not >> continue
                "show_puff_details" -> do
                    p <- readVal selPuff
                    case p of
                        Nothing -> setMessage "No puff selected" >> continue
                        Just p -> do
                            pdw <- puffDetailsWidget p
                            setRenderWidget rc (keyCatcherWidget (\_ -> setRenderWidget rc fw >> return True) pdw) >> continue
                "new_puff" -> withEditing $ composePuff (setRenderWidget rc fw)[] [] >> continue
                "modify_presence_string" -> do
                    p <- readVal myPresence
                    (ys,_) <- scrSize
                    v <- commandRead justGetKey stdScr (ys - 1) "Presence: " p
                    case v of
                        Just v -> writeVal myPresence v >> setMessage ("Presence updated: " ++ v)
                        Nothing -> setMessage "Presence unchanged"
                    continue
                "reply_to_author" -> withEditing $ do
                    p <- readVal selPuff
                    case p of
                        Nothing -> return ()
                        Just p -> do
                            pkw <- fmap (concat . (map words)) $ configLookupList "PRESERVED_KEYWORDS"
                            let kws = filter (`elem` (map packString pkw)) (getFragmentStrings p f_messageKeyword)
                            composePuff(setRenderWidget rc fw) [catParseNew (getAuthor p)] kws
                    continue
                "goto_match" -> do
                    p <- readVal selPuff
                    case p of
                        Just p | Just b <- getFragmentString p f_messageBody -> let
                            v = if null mw then continue else setRenderWidget rc (stackedWidgets [keyCatcherWidget pk  (dialog tw), fw]) >> return True
                            cs = ['a' .. 'z'] ++ ['A' .. 'Z']
                            mw = zip cs $ matchWords matchTable (unpackPS b)
                            tw = unlines (map (\(a,(_,b)) -> a :' ':b)  mw)
                            --pk x = do
                            --    setRenderWidget rc fw
                            --    return True
                            pk x = case x of
                                        (KeyChar n) | Just (a,_) <- lookup n mw -> do
                                            --rawSystem a []
                                            mySystem a
                                            return ()
                                        key -> keyError "Unknown match" key
                                    >> do
                                    setRenderWidget rc fw
                                    return True
                              in v
                        _ -> continue

                "follow_up" -> withEditing $ do
                    p <- readVal selPuff
                    case p of
                        Nothing -> setMessage "No puff selected"
                        Just p -> do
                            pkw <- fmap (concat . (map words)) $ configLookupList "PRESERVED_KEYWORDS"
                            let kws = filter (`elem` (map packString pkw)) (getFragmentStrings p f_messageKeyword)
                            composePuff (setRenderWidget rc fw) (cats p) kws
                    continue
                "group_reply" -> withEditing $ do
                    p <- readVal selPuff
                    case p of
                        Nothing -> setMessage "No puff selected"
                        Just p -> do
                            pkw <- fmap (concat . (map words)) $ configLookupList "PRESERVED_KEYWORDS"
                            let kws = filter (`elem` (map packString pkw)) (getFragmentStrings p f_messageKeyword)
                            composePuff (setRenderWidget rc fw) (nub $ catParseNew (getAuthor p):cats p) kws
                    continue
                "resend_puff" -> withEditing $ do
                    p <- readVal selPuff
                    case p of
                        Nothing -> setMessage "No puff selected"
                        Just p -> do
                            pcw <- puffConfirm gc (setRenderWidget rc fw) p ic
                            setRenderWidget rc pcw
                    continue
		"redraw_screen" -> attempt (touchWin stdScr)  >> resizeRenderContext rc (return ()) >> continue
		"show_help_screen" -> setRenderWidget rc (keyCatcherWidget (\_ -> setRenderWidget rc fw >> return True) hw) >> continue
		"show_status_screen" -> setRenderWidget rc (keyCatcherWidget (\_ -> setRenderWidget rc fw >> return True) statusWidget) >> continue
		"show_presence_status" -> setRenderWidget rc (keyCatcherWidget (\_ -> setRenderWidget rc fw >> return True) presenceWidget) >> continue
                act -> setMessage ("Unknown action: " ++ act ++ " -- " ++ helpText) >> continue
	puffDetailsWidget p = do
	    (_,xs) <- scrSize
	    let pv = newSVarWidget presence_r (presenceViewUser (getAuthor p))
            wr <- case fmap unpackPS (getFragmentString p (f_messageId)) of
                    Nothing -> return ""
                    Just mid ->  Hash.lookup puffRRHash mid >>= \ml -> case ml of
                        Nothing -> return ""
                        Just ml -> return $ "\nReceieved by:\n" ++ unlines (snub ml)
	    v <- widgetScroll (widgetVerticalBox False [(NoExpand,widgetText $ chunkText (xs - 1) ((showPuff p) ++ wr)), (NoExpand, widgetText "\n\n\n"), (NoExpand, pv)])
	    return v
	justGetKey = readChan ic >>= \v -> case v of
	    (MainEventPuff p) -> addPuff p >> justGetKey
	    (MainEventKey k) -> do
		    getClockTime >>= writeVal userActionTime
		    return $ keyCanon k
	    _ -> writeChan icmain v >> justGetKey
	nextKey = do
	    ce <- isEmptyChan ic
	    when ce $ tryDrawRenderContext rc
	    cemain <- isEmptyChan icmain
	    v <- if cemain then readChan ic else readChan icmain
	    case v of
		(MainEventPuff p) -> do
                    v <- is_at_end
		    addPuff p
                    when v scroll_end
		    s <- configLookupElse "ON_INCOMING_PUFF" ""
		    mapM_ (processKey mwidget) (stringToKeys s)
		    nextKey
		(MainEventKey KeyResize) ->  nextKey
		(MainEventKey k) -> do
		    getClockTime >>= writeVal userActionTime
		    b <- keyRenderContext rc $ keyCanon k
		    if b then nextKey else return ()
		(MainEventComposed ep done) -> do
		    case ep of
			Nothing -> setMessage "Puff cancelled"
			Just p -> do
			    pcw <- puffConfirm gc done p ic
			    setRenderWidget rc pcw
		    writeVal editing_r False
		    nextKey
    np <- configLookupBool "NO_PRESENCE_NOTIFY"

    done <- if np then return (return ()) else do
        gonePresencePuff >>= galeWillPuff gc
        ploopID <- forkIO presenceLoop
        return $ killThread ploopID
    forkIO $ let f n = waitJVar myPresence  n >>= \n' -> sendPresence True >> f n' in f presence
    setRenderWidget rc fw
    nextKey
    done

getMyNotifyCategory = do
    Category (n,d) <- fmap catParseNew getGaleId
    return $ Category (("_gale.notice." ++ n),d)

-- makeURLRegex :: IO Regex
-- makeURLRegex = regcomp "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?" regExtended


-- rendered puffs height
puffHeight :: Puff
    -> Int -- ^ Width of screen in characters
    -> Int -- ^ height of puff when rendered
puffHeight (Puff {fragments = frags}) mw = n + 2 where
    body' = maybe [] (lines . paragraphBreak mw . expandTabs ) $ fmap unpackPS (getFragmentString frags  f_messageBody)
    n = length body'

renderPuff :: Puff
    -> Int  -- ^ width of screen in characters
    -> Window -- ^ where to draw puff
    -> Int
    -> (Int,Int)
    -> Bool
    -> (String -> String)
    -> IO ()

renderPuff p@(Puff {cats =cats, fragments = frags}) mw w y (_,ys) selected textFilter = doit where
    sender = maybe "" unpackPS $ getFragmentString p f_messageSender
    body' = maybe [] (lines . paragraphBreak mw . expandTabs . textFilter) $ fmap unpackPS (getFragmentString frags  f_messageBody)
    Just time = getFragmentTime frags f_idTime `mplus` getFragmentTime frags (fromString "_ginsu.timestamp")
    kwds = getFragmentStrings p f_messageKeyword
    n = length body'
    addCat w (Category (x,y)) = withColor w (Pair c) $ Curses.withAttr w attrBold (waddstr w x) >> waddstr w ('@':y)  where
        c = fromIntegral $ (hashPS (packString (x ++ "@" ++ y)) `mod` 7) + 1
    doit = do
        let ac = Pair $ (fromIntegral $ (hashPS $ packString (getAuthor p)) `mod` 7) + 1
	when (y >= 0 && y < ys) $ do
	    wmove w y 0
	    when selected $ standout >> return ()
	    sequence (intersperse (space w) $ map (addCat w) cats)
	    when selected $ standend >> return ()
	    space w >> space w
	    when selected $ standout >> return ()
            waddstr w (unwords (map (('/':) . unpackPS)  kwds))
            Just fmt <- configLookup "PUFF_DATE_FORMAT"
            st <-  fmap (formatCalendarTime defaultTimeLocale fmt) $ toCalendarTime time
            let lst = length st
            wAttrOn w attrBold
            withColor w ac $ mvwaddstr w y (mw - length sender - lst - 1) sender
            wAttrOff w attrBold
            mvwaddstr w y (mw - lst) st
            when selected $  standend >> return ()
        mapM_ (\(p,s) ->  when (y + p < ys) (mvwaddstr stdScr (y + p) 0 s)) (zip [1..] $ body')
        --let bs = (if selected then "======" ++ [rTee] else replicate 6 hLine ++ [rTee])
        let bs = (if selected then "======" ++ [rTee] else replicate 6 hLine ++ [rTee]) ++ getAuthor p ++ [lTee] ++ cycle (if selected then "=" else [hLine])
        --let es =  [lTee] ++ cycle (if selected then "=" else [hLine])
        --when (y + 1 + n < ys) $ mvwaddstr w (y + 1 + n) 0 (take mw $ cycle $ if selected then "=" else  "-")
        when selected $ wAttrOn w attrBold
        when (y + 1 + n < ys) $ do
            mvwaddstr w (y + 1 + n) 0 (take mw bs)
            --mvwaddstr w (y + 1 + n) 0 (bs)
            --withColor w ac $ mvwaddstr w (y + 1 + n) (length bs) (getAuthor p)
            --mvwaddstr w (y + 1 + n) (length bs + length (getAuthor p)) (take mw es)
        when selected $ wAttrOff w attrBold
        return ()




------------------
-- Curses routines
------------------

waddstr w s = Control.Exception.try (wAddStr w s) >> return ()
mvwaddstr w y x s = Control.Exception.try (mvWAddStr w y x s) >> return ()
wmove w y x = Control.Exception.try (wMove w y x) >> return ()

space w = waddstr w " "

--------------
-- help screen
--------------


helpWidget = do
    helpTable <- getHelpTable
    let w = widgetText helpTable
        s = "Help Screen"
        v = "* scroll with direction keys - any other key to return to main screen *"
    sw <- widgetScroll w
    return $ widgetVerticalBox False [(NoExpand,widgetAttr [AttrBold] (widgetText s)),(NoExpand,widgetText v),(ExpandFill, widgetSimpleFrame sw)]

statusWidget = do
    let gs = do
            st <- Status.getStatus
            ls <- Status.getLog
            return $  concat ([st, "\n--- Log ---\n"] ++ ls)
    let w = dynamicWidget $  fmap widgetText gs
        s = "Status Screen"
        v = "* scroll with direction keys - any other key to return to main screen *"
    sw <- widgetScroll w
    return $ widgetVerticalBox False [(NoExpand,widgetAttr [AttrBold] (widgetText s)),(NoExpand,widgetText v),(ExpandFill, widgetSimpleFrame sw)]

-------------------
-- Puff composition
-------------------

withPrivateFiles action = do
    om <- Posix.setFileCreationMask (Posix.groupModes `Posix.unionFileModes` Posix.otherModes)
    v <- action
    Posix.setFileCreationMask om
    return v

noBodyWords fl = [x|x@(n,_) <- fl, n /= f_messageBody, n /= f_messageKeyword]

withNBRWorkaround f = do
    System.IO.stdin `seq` return ()
    PosixIO.setFdOption 0 PosixIO.NonBlockingRead False
    f
    PosixIO.setFdOption 0 PosixIO.NonBlockingRead False

mySystem s = do
    putLog LogInfo $ "system " ++ show s
    withNBRWorkaround $ withProgram $ System.Cmd.system s

myRawSystem e s = do
    withNBRWorkaround $ withProgram $ System.Cmd.rawSystem e s

editPuff :: Puff -> Chan MainEvent -> IO () -> IO ()
editPuff puff ic done = do
    e <- getEditor
    fn <- getTmpFile
    eob <- configLookupList "EDITOR_OPTION"
    eonew <- configLookupList "EDITOR_NEWPUFF_OPTION"
    let eo = if null (cats puff) then eonew else eob
    let it = ["To: " ++ simpleQuote (showDestination (cats puff) (map unpackPS $ getFragmentStrings puff f_messageKeyword)) , "--------"]
    let mb = case fmap unpackPS (getFragmentString puff f_messageBody) of
            Just mb -> mb
            Nothing -> "\n"
    withPrivateFiles $ writeRawFile fn (stringToBytes $ unlines it ++ mb)
    putLog LogInfo $ "system: " ++  (e ++ " " ++ unwords eo ++ " " ++ shellQuote [fn])
    bgedit <- configLookupBool "BACKGROUND_EDIT"
    bgcmd <- if bgedit then configLookupList "BACKGROUND_COMMAND" else return []
    let (cmd:args) = (concatMap words bgcmd) ++ [e] ++ (concatMap words eo) ++ [fn]
    let after = editPuffDone fn ic done it puff
    let handle = do st <- try $ Posix.getAnyProcessStatus False False
                    case st of Right (Just _) -> handle
                               _ -> after
    if bgedit then do
            Posix.installHandler Posix.sigCHLD (Posix.CatchOnce handle) Nothing >> return ()
            Posix.forkProcess (Posix.executeFile cmd True args Nothing) >> return ()
        else do
            myRawSystem cmd args
            after

editPuffDone :: String -> Chan MainEvent -> IO () -> [String] -> Puff -> IO ()
editPuffDone fn ic done it puff = do
    pn <- fmap (lines . bytesToString)$ readRawFile fn
    handleMost (\_ -> return ()) (removeFile fn)
    ep <- if not (length pn > 1 && pn /= it) then return Nothing else do
        let (cs',kwds') = readDestination (drop 4 (head pn))
        ncats <- expandAliases (cs')
        tw <- configLookupBool "TRIM_BLANKLINES"
        body <- return $ if not tw then (unlines $ (drop 2 pn)) else
            trimBlankLines (unlines $ (drop 2 pn))
        return $ if body == "" then Nothing else
                Just puff { cats = ncats, fragments = noBodyWords (fragments puff) ++ [(f_messageBody,FragmentText (packString body))] ++ [ (f_messageKeyword,FragmentText (packString k)) | k <- kwds']}
    writeChan ic $ MainEventComposed ep done

showDestination cs kwds = (map catShowNew cs ++ map ('/':) kwds)
readDestination :: String -> ([Category],[String])
readDestination s = splitEither (map pi (simpleUnquote s)) where
    pi ('/':s) = Right s
    pi x = Left (catParseNew x)

--------------------
-- Puff confirmation
--------------------

anonymousFragments = map (liftT2 (fromString,\x -> FragmentText (packString x))) [("id/class", "anonymous"), ("id/instance",  "anonymous"), ("message/sender",  "Anonymous")]

minusFrag fl s = [x |x@(n,_) <- fl, n /= s]

prettyPuff puff = unlines xs ++  body where
    to = ["To: " ++ simpleQuote (showDestination (cats puff) (map unpackPS $ getFragmentStrings puff f_messageKeyword)) ]
    from = ["From: " ++ unpackPS t| (n,FragmentText t) <- fragments puff,  n == f_messageSender]
    body = case [ unpackPS t | (n,FragmentText t) <- fragments puff, n == f_messageBody] of
        (t:_) -> t
        [] -> ""
    rr = if hasFragment puff (f_questionReceipt) then ["Return receipt: Yes"] else []
    xs = to ++ from ++ rr ++ ["-------"]


puffConfirm ::  GaleContext -> IO () -> Puff -> Chan MainEvent -> IO Widget
puffConfirm gc done puff ic = do
    (_,xs) <- scrSize
    gid <- getGaleId
    ncats <- expandAliases (cats puff)
    puff <- return $ puff {cats = ncats}
    psv <- newMVar puff
    let Category (n,d) = catParseNew gid
    let rr_cat = case getFragmentString puff (f_messageId) of
            Just mid -> "_gale.rr." ++ n ++ "." ++ unpackPS mid ++ "@" ++ d
            Nothing -> gid

    gs <- fmap (concat . map words) $ configLookupList "GALE_SUBSCRIBE"
    when  (not (or [c `subCategory` catParseNew g | c <- cats puff, g <- gs] ) || packString "ping" `elem` getFragmentStrings puff f_messageKeyword) $
            writeVal psv (puff {fragments = (f_questionReceipt, FragmentText (packString rr_cat)) : fragments puff})
    sbsv <- newMVar ""
    psvs <- newMVar (prettyPuff, showPuff)
    csv <- combineVal psv psvs
    let pw = newSVarWidget csv (\(p,(f,_)) -> dynamicWidget (verifyDestinations gc (cats p) >>= \text -> return $ widgetText $ text ++ "--\n" ++ (chunkText (xs - 4) (f p))))
    let w = (widgetCenter $  widgetText "Send puff?")
        h = (widgetCenter $ widgetText (paragraph xs $ "y:send q:cancel e:edit r:returnReceipt h:allHeaders A:anonymize <C-o>:rot13" ))
        pk (KeyChar 'q')  = done >> return True
        pk (KeyChar 'n')  = done >> return True
        pk (KeyChar 'r') = do
            p <- readVal psv
            if hasFragment p (f_questionReceipt) then
                writeVal psv (p {fragments = [x | x@(n,_) <- fragments p , n /= f_questionReceipt]})
                    else writeVal psv (p {fragments = (f_questionReceipt, FragmentText (packString rr_cat)) : fragments p})
            return True
        pk (KeyChar 'h') = do
            mapVal psvs (\(x,y) -> (y,x))
            return True
        pk (KeyChar 'A') = do
            p <- readVal psv
            if hasFragment p (f_messageId) then do
                writeVal psv (p {signature = [], fragments = anonymousFragments `mergeFrags` (fragments p `minusFrag` f_messageId)})
                    else do
                        pt <- puffTemplate
                        writeVal psv (p {signature = signature pt, fragments = (fragments pt `minusFrag` f_idTime) `mergeFrags` fragments p})
            return True
        pk (KeyChar 'y')  = do
            puff <- readVal psv
            galeSendPuff gc puff
            done
            return True
        pk (KeyChar 'e') = do
            puff <- readVal psv
            done
            editPuff puff ic done
            return True
        pk (KeyChar '\x0F') = do
            p <- readVal psv
            case fmap unpackPS (getFragmentString p f_messageBody) of
                Just mb -> writeVal psv (p {fragments = [(f_messageBody, FragmentText(packString $ rot13 mb))] `mergeFrags` (fragments p `minusFrag` f_messageBody)})
                Nothing -> return ()
            return True
        pk key = do
            writeVal sbsv $  "Invalid keystroke: " ++ keysToString [key]
            return True
    pb <- widgetScroll $ pw
    return $ keyCatcherWidget pk $ widgetVerticalBox False [(NoExpand, w), (NoExpand, widgetAttr [AttrBold] h), (ExpandFill, widgetSimpleFrame pb),(NoExpand, simpleStatusBarWidget sbsv)]

simpleStatusBarWidget svm  = widgetAttr [AttrBold] $ newSVarWidget svm widgetText




commandRead :: Monad m => IO Curses.Key -> Window -> Int -> String -> String -> IO (m String)
commandRead ic win yloc prompt init = withCursor CursorVisible  $ cr (length init) (reverse init)
    where
    l n v = min (max n 0) (length v)
    cr cloc v = pc cloc v >> ic >>= \x -> case x of
	(KeyChar '\n') -> return $ return (reverse v)
	(KeyChar '\r') -> return $ return (reverse v)
	(KeyEnter) -> return $ return (reverse v)
	(KeyChar '\b') -> let z = (let (a,b) = splitAt (length v - cloc) v in a ++ drop 1 b) in  cr (l (cloc - 1) z) z
	(KeyBackspace) -> let z = (let (a,b) = splitAt (length v - cloc) v in a ++ drop 1 b) in  cr (l (cloc - 1) z) z
        (KeyHome) -> cr 0 v
        (KeyEnd) -> cr (length v) v
        (KeyLeft) -> cr (l (cloc - 1) v) v
        (KeyRight) -> cr (l (cloc + 1) v) v
        (KeyChar '\x0B') -> cr cloc (drop (length v - cloc) v)
        (KeyChar '\BEL') -> return (fail "")
	(KeyChar c) -> cr (cloc + 1) (let (a,b) = splitAt (length v - cloc) v in a ++ [c] ++ b)

	_ -> return (fail "unknown key")
    pc cloc v = do
	wmove win yloc 0
	wClrToEol win
	wAttrOn win attrBold
	waddstr win prompt
	wAttrOff win attrBold
	waddstr win (reverse v)
	wmove win yloc (cloc + length prompt)
	refresh


