module Deploy where

import Deployment
import State

import Control.Concurrent.MVar
import WASH.CGI.RawCGI as RawCGI

addDeployment :: DeployedObject -> IO ()
addDeployment dobj = do
  st0 <- takeMVar currentState
  let ds = deploymentState st0
      dobjs = objs ds
      st1 = st0 { deploymentState = ds { objs = dobj : dobjs } }
  putMVar currentState st1

activate :: [String] -> IO ()
activate names = 
  modify names True

passivate :: [String] -> IO ()
passivate names =
  modify names False

modify :: [String] -> Bool -> IO ()
modify names newValue = do
  st0 <- takeMVar currentState
  let ds = deploymentState st0
      dobjs = map actOn (objs ds)
      st1 = st0 { deploymentState = ds { objs = dobjs } }
      actOn dobj = if objectName dobj `elem` names && not (objectSystem dobj)
		   then dobj { objectActive = newValue }
		   else dobj
  putMVar currentState st1

undeploy :: [String] -> IO ()
undeploy names =  do
  st0 <- takeMVar currentState
  let ds = deploymentState st0
      dobjs = filter testOn (objs ds)
      st1 = st0 { deploymentState = ds { objs = dobjs } }
      testOn dobj = objectName dobj `elem` names
  putMVar currentState st1

modifyOptions :: [String] -> (CGIOptions -> CGIOptions) -> IO ()
modifyOptions names converter = do
  st0 <- takeMVar currentState
  let ds = deploymentState st0
      dobjs = map actOn (objs ds)
      st1 = st0 { deploymentState = ds { objs = dobjs } }
      actOn dobj = if objectName dobj `elem` names
		   then dobj { objectOptions = converter (objectOptions dobj) }
		   else dobj
  putMVar currentState st1

