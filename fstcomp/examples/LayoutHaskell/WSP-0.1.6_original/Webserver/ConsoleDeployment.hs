module ConsoleDeployment where

import Console (console)
import Deployment
import WASH.CGI.CGI as CGI (makeServlet)
import WASH.CGI.CGITypes as CGITypes

mkConsole :: String -> DeployedObject
mkConsole absPathPrefix = 
  DeployedObject 
    { objectName = "System console"
    , objectPath = absPathPrefix
    , objectMod = "*internal*"
    , objectVal = makeServlet console
    , objectActive = True
    , objectSystem = True
    , objectOptions = [SessionMode LogOnly]
    }
