module Dispatch where

import Control.Concurrent.MVar
import Maybe
import Monad

import Deployment
import State 
import WASH.CGI.CGI
import WASH.CGI.RawCGI

-- | takes the server configuration and the current absolute access path and
-- returns a pair of a string and the cgi program (wrapped in the Maybe
-- monad). The string is the suffix after stripping the application prefix.
runDispatch :: a -> String -> IO (Maybe (String, CGIOptions, CGIProgram))
runDispatch conf path =
  do st <- readMVar currentState
     let deployments = deploymentState st
         dobjs = objs deployments
     return $ dispatchLoop path dobjs

-- | Checks the request path against the list of application prefixes of the
-- deployed objects. Returns the path suffix and the cgi program, if
-- successful. 
dispatchLoop :: String -> [DeployedObject] -> Maybe (String, CGIOptions, CGIProgram)
dispatchLoop path [] = 
  Nothing
dispatchLoop path (dobj : dobjs) =
  do if objectActive dobj then return () else Nothing
     pathInfo <- splitPath (objectPath dobj) path
     return (pathInfo, objectOptions dobj, objectVal dobj)
  `mplus`
  dispatchLoop path dobjs

-- | Attempts to match a path prefix against a test path and returns the suffix
-- of the test path
splitPath :: String -> String -> Maybe String
-- splitPath pathPrefix testPath
splitPath "" testPath =
  if legalSuffix testPath 
  then Just testPath
  else Nothing
splitPath _ "" =
  Nothing
splitPath (p:ps) (t:ts) =
  if p == t
  then splitPath ps ts
  else Nothing
  
-- 
legalSuffix :: String -> Bool
legalSuffix "" = True
legalSuffix (x:xs) = legalSuffixChar x

legalSuffixChar :: Char -> Bool
legalSuffixChar x = x `elem` "/?"
