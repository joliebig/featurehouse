module State where

import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.MVar

import Deployment

data State =
    State
      { deploymentState :: DeploymentState
      }

currentState :: MVar State
currentState = unsafePerformIO (newMVar initialState)

initialState :: State
initialState =
  State
    { deploymentState = initialDeploymentState
    }
