module Compile where

import qualified List
	       
import qualified System.Plugins as P

import WASH.CGI.CGI as CGI (makeServlet)
import WASH.CGI.CGITypes as CGITypes

import Deployment 
import Util

type Errors = [String]

compileOptions :: Descriptor -> [String]
compileOptions dep =
  -- done by hs-plugins:
  -- let dd = deploymentDirectory dep
  -- in [ "-i" ++ dd, "-odir", dd, "-hidir", dd ]
  -- ++

  -- turn on the WASH preprocessor
  -- preprocessing could be handled by haskell-src-ext directly
  "-pgmF" : "/usr/local/bin/wash2hs" : "-F" :

  -- use the WashNGo package for compiling 
  -- it might not be necessary for linking, but how do I turn it off?
  let pkgs = "WashNGo" : packages dep in

  List.concatMap (\ pkgName -> ["-package", pkgName]) pkgs
  ++
  compilerFlags dep

moduleTemplate modToImport nameToCall =
  ["module XXXXXXXX where"
  ,"import " ++ modToImport ++ "(" ++ nameToCall ++ ")"
  ,"import WASH.CGI.CGI (makeServlet)"
  ,"xxxxxxxx = makeServlet " ++ nameToCall
  ]

compileServlet :: Descriptor -> IO (Either Errors DeployedObject)
compileServlet dep =
  let options = compileOptions dep 
      objDir  = deploymentDirectory dep
      moduleToImport = mainModule dep
      symbolToCall = mainSymbol dep
      moduleToLoad = objDir ++ "/XXXXXXXX.hs"
      symbolToLoad = "xxxxxxxx"
  in
  trace ("Compile.compileServlet using "++show options) $
  do writeFile moduleToLoad (unlines $ moduleTemplate moduleToImport symbolToCall)
     s <- P.makeAll moduleToLoad options

     let loadObject obj = do
	    l <- P.load obj [objDir] [] symbolToLoad
	    case l of
	      P.LoadSuccess mod val -> do
		trace ("Compile.compileServlet/LoadSuccess "
		       ++P.path mod++" "++ moduleToImport) $ return ()
		return $ Right $
		  DeployedObject { objectName = applicationName dep
				 , objectPath = applicationPath dep
				 , objectMod  = moduleToImport
				 , objectVal  = val
				 , objectActive = False
				 , objectSystem = False
				 , objectOptions = [SessionMode LogOnly]
				 }
	      P.LoadFailure e ->
		trace ("Compile.compileServlet/LoadFailure "++show e) $ do 
	      return $ Left $ e

     case s of
	    P.MakeSuccess P.NotReq obj ->
	      trace ("Compile.compileServlet/MakeSuccess NotReq: "++show obj) $ do
	      loadObject obj

	    P.MakeSuccess P.ReComp obj ->
	      trace ("Compile.compileServlet/MakeSuccess ReComp: "++show obj) $ do
	      loadObject obj

	    P.MakeFailure e ->
	      trace ("Compile.compileServlet/MakeFailure: "++show e) $ do
	      return $ Left $ e

	    _ ->
	      trace ("Compile.compileServlet/UNKNOWN") $ do
	      return $ Left $ ["unknown outcome of Plugins.makeAll"]

