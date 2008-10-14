module GinsuConfig(
    checkConfigIsGood,
    configureGinsu,
    doCheckConfig,
    getGaleProxy,
    getGaleId,
    getGaleDomain,
    getGaleAliases,
    galeFile,
    getEditor
    ) where

import ConfigFile
import Control.Exception
import Data.Monoid
import Directory
import ErrorLog
import ExampleConf
import Gale.Gale
import Gale.Puff
import GenUtil
import Monad
import System
import System.Posix as Posix hiding (getEnv)
import System.Posix.Files

checkConfigIsGood = do
    gd <- configLookup "GALE_DOMAIN"
    case gd of
        Just _ -> return ()
        Nothing -> putStrLn "GALE_DOMAIN is not set! either set $GALE_DOMAIN in the enviornment or set it in ~/.gale/conf or ~/.gale/ginsu.conf\n" >> doCheckConfig

configFileG xs = mapConfig ("GINSU_" ++) (configFile xs) `mappend` configFile xs

configureGinsu = do
    galeDir <- getGaleDir
    configSetup $ mconcat
            [mapConfig ("GINSU_" ++) configEnv,
            configFileG (galeDir  ++ "ginsu.config"),
            configEnv,
            configFileG (galeDir ++ "conf"),
            configDefault (parseConfigFile exampleConf),
            configBuilder]

doCheckConfig = do
    galeDir <- getGaleDir


    gp <- getGaleProxy
    gid <- configLookupElse "GALE_ID" "* Not Found *"
    galeDomain <- configLookupElse "GALE_DOMAIN" "* Not Found *"
    galeAliases <- getGaleAliases
    galeSubscribe <- configLookupList "GALE_SUBSCRIBE"


    mapM_ putStrLn $ buildTableLL [
	( "GALE_DIR ",  galeDir),
	( "GALE_DOMAIN ", galeDomain),
	( "GALE_ID ", gid),
	( "GALE_PROXY ", simpleQuote gp),
	( "GALE_SUBSCRIBE ", unwords (snub galeSubscribe)) ]
    when (galeAliases /= []) $ putStrLn $ "GALE_ALIASES \n" ++
	unlines ( map  (\ (l,cat) -> "  " ++ l ++ " -> " ++ show cat)  galeAliases)

    let p = (galeDir ++ "/auth/private/")
	knames = [p ++ gid, p ++ gid ++ ".gpri"]
    gn <- (mapM (\fn -> (doesFileExist fn >>= \b -> if b then return (Just fn) else return Nothing)) knames)
    case msum gn of
	Just fn -> putStrLn $ "Private key found in: " ++ fn
	Nothing -> putStrLn $ "Private key for '" ++ gid ++ "' not found in " ++ p
    putStrLn ""
    --putStrLn showKeyInfo
    --kt <- buildKeyTable
    --print kt
    exitSuccess

configBuilder = toConfig cb where
    cb "GALE_ID" = do
        n <- first [getEnv "LOGNAME", getEnv "USER", Posix.getLoginName]
        d <- configLookup "GALE_DOMAIN"
        case d of
            Just d -> return [("<LOGIN>@$GALE_DOMAIN",("GALE_ID", n ++ "@" ++ d))]
            Nothing -> return []
    cb _ = return []

getGaleProxy :: IO [String]
getGaleProxy = do
    gps <- configLookup "GALE_PROXY"
    case gps of
	Nothing -> do
	    v <- configLookup "GALE_DOMAIN"
            case v of
                Just v -> return $ hostStrings v
                Nothing -> return []
	(Just x) -> return (words x)


getGaleId :: IO String
getGaleId = do
    v <- configLookup "GALE_ID"
    case v of
        Just x -> return x
        Nothing -> return ""

getGaleDomain :: IO String
getGaleDomain = do
    v <- configLookup "GALE_DOMAIN"
    case v of
        Just x -> return x
        Nothing -> do
            gid <- getGaleId
            return $ drop 1 (dropWhile (/= '@') gid)


getGaleAliases :: IO [(String,Category)]
getGaleAliases = do
    v <- galeFile "aliases/"
    fs <- handle (\_ -> return []) $ getDirectoryContents v
    --putLog LogDebug $ "aliases/ " ++ show fs
    let f fn = do
	--fc <- first [readFile (v ++ fn), readSymbolicLink (v ++ fn)]
        fc <- readAlias (v ++ fn)
        --putLog LogDebug $ "readAlias " ++ (v ++ fn) ++ " " ++ fc
	return (fn, catParseNew fc)
    ks <- mapM (\x -> tryMost (f x)) fs
    return [x | (Right x) <- ks]


readAlias :: String -> IO String
readAlias fn = getSymbolicLinkStatus fn >>= \s -> if isRegularFile s then readFile fn else
    (if isSymbolicLink s then  readSymbolicLink fn else fail "bad alias")



varsLookupElse :: [String] -> IO String -> IO String
varsLookupElse ss action = do
    v <- fmap concat $ mapM configLookupList ss
    case v of
        (x:_) -> return x
        _ -> action

galeFile :: String -> IO String
galeFile fn = do
    gd <- getGaleDir
    return (gd ++ fn)

getEditor = varsLookupElse ["VISUAL", "EDITOR"] (return "vi")
