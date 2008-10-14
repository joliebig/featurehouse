module InitialDeployments where

import ConsoleDeployment
import Deploy

init :: IO ()
init = do
  addDeployment (mkConsole "/console")

