module Deployment where

import System.Plugins as P

import WASH.CGI.RawCGI as RawCGI (CGIProgram, CGIOptions)

-- Deployed application is accessible through
-- http://server_fqdn/<anchor>
-- ==> just one entry point, multiple entry points through customizable
-- dispatcher module

data DeploymentState = 
    DeploymentState
      { desc :: [Descriptor]		    -- ^ uploaded
      , objs :: [DeployedObject]	    -- ^ compiled and loaded
      }

initialDeploymentState =
  DeploymentState
    { desc = []
    , objs = []
    }

data Descriptor =
    Descriptor 
      -- user stuff
      { applicationName :: String	-- ^ as defined through deployment intf
      , applicationDescription :: String -- ^ descriptive text
      , applicationPath :: String	-- ^ anchor in web server's namespace,
				        -- prefix of abspath component in request URI
      -- system stuff
      , deploymentDirectory :: FilePath	-- ^ chosen randomly by deployment tool
      , extra_resources :: [String]	-- ^ files required by application (images, etc)
      , mainModule :: String		-- ^ module containing the entry point
      , mainSymbol :: String		-- ^ symbol for the entry point
      , sources :: [String]		-- ^ uploaded source files (to be compiled)
      , libraries :: [String]		-- ^ uploaded libraries (for linking)
      , extra_libraries :: [String]	-- ^ further libraries (for linking)
      , packages :: [String]		-- ^ required packages
      , compilerFlags :: [String]	-- ^ for compiling source -> object
      , linkerFlags :: [String]		-- ^ for linking into the server
      }
    deriving (Read, Show)

data DeployedObject =
    DeployedObject
      { objectName :: String		-- ^ corresponding to applicationName
      , objectPath :: String	        -- ^ corresponding to applicationPath
      , objectMod :: String		-- ^ object module name
      , objectVal :: CGIProgram		-- ^ symbol to load
      , objectActive :: Bool		-- ^ flag if active
      , objectSystem :: Bool		-- ^ flag if system object
      , objectOptions :: CGIOptions	-- ^ set options on deployment
      }
