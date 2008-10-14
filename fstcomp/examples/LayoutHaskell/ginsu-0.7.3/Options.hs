module Options(
    Env(..),
    ginsuOpts
    ) where

import Curses
import ErrorLog
import ExampleConf
import GenUtil
import GinsuConfig
import Help
import Gale.KeyCache
import KeyName
import System
import System.Console.GetOpt
import Version

-------------------------
-- options/initialization
-------------------------
data Env = Env {
    envAction :: IO (),
    envVerbose :: Int,
    envConfig :: (Maybe String),
    envJustArgs :: Bool,
    envErrorLog :: Maybe String,
    envNoPufflog :: Bool,
    envNoWritePufflog :: Bool
    }


env = Env {
    envAction = return (),
    envVerbose = 0,
    envConfig = Nothing,
    envJustArgs = False,
    envNoPufflog = False,
    envNoWritePufflog = False,
    envErrorLog = Nothing
}


options :: [OptDescr (Env -> Env)]
options = [
    Option ['v']  ["verbose"] (NoArg (\e->e{envVerbose = envVerbose e + 1})) "increase verbosity output to errorlog.",
    Option ['V']  ["version"] (NoArg (doJust (putStrLn fullName))) "print version information",
    --	Option ['c']  ["config"] (ReqArg (\x e->e{envConfig = (Just x)}) "FILE") "use this configuration file",
    Option ['s']  ["sample-config"] (NoArg (doJust (putStr exampleConf))) "print sample configuration file to stdout",
    Option ['m']  ["man"] (NoArg (doJust doMan)) "print all internal help screens to stdout",
    Option ['e']  ["justargs"] (NoArg (\e->e{envJustArgs = True})) "only subscribe to command line arguments",
    Option ['P']  [] (NoArg (\e->e{envNoWritePufflog = True})) "do not write to pufflog",
    Option []  ["help"] (NoArg $ doJust (putStrLn usage)) "show this help screen",
    Option []  ["nopufflog"] (NoArg (\e->e{envNoPufflog = True})) "do not read or write pufflog",
    Option []  ["errorlog"] (ReqArg (\x e->e{envErrorLog = Just x}) "FILE") "log errors to file",
    Option []  ["dumpkey"] (ReqArg (\x -> doJust (dumpKey x)) "KEYFILE") "print info for keyfile",
    Option []  ["checkconfig"] (NoArg (doAction doCheckConfig)) "check and print out configuration"
    ]

privateOptions = [
    Option []  ["testcurses"] (NoArg (doJust cursesTest)) "print curses diagnostics",
    Option []  ["showOptions"] (NoArg (doJust showOptions)) "show options",
    Option []  ["showKeys"] (NoArg (doJust (getKeyHelpTable (0,0) >>= putStr))) ""
    ]


{-# NOINLINE ginsuOpts #-}
ginsuOpts :: IO (Env,[String])
ginsuOpts = do
    args <- getArgs
    r@(env,_) <- case (getOpt Permute (options ++ privateOptions) args) of
	   (as,n,[]) -> return (foldr ($) env as ,n)
    	   (_,_,errs) -> putErrDie (concat errs ++ usage)
    case (envVerbose env) of
	1 -> do
	    setLogLevel LogInfo
	    putLog LogNotice $ "Verbosity Level: Info"
	n | n > 1 -> do
	    setLogLevel LogDebug
	    putLog LogNotice $ "Verbosity Level: Debug"
	_ -> return ()
    envAction env
    return r

doMan = getHelpTable >>= putStrLn

usage = usageInfo usageHeader options ++ usageTrailer

doAction :: IO a -> (Env -> Env)
doAction a = \e -> e {envAction = a >> envAction e}

doJust action = doAction (action >> exitSuccess)

showOptions = putStr $ "|||| Available Options ||\n" ++ concatMap f options where
    f (Option so los as hm) = "|| " ++ concatInter ", " (map ga (so' ++ los')) ++ " || "  ++ hm ++ "||\n" where
        so' = map (\c -> '-':[c]) so
        los' = map (\cs -> '-':'-':cs) los
        ga = case as of
            ReqArg _ a  -> (++ (" <i>&lt;" ++ a ++ "&gt;</i>"))
            _ -> id


