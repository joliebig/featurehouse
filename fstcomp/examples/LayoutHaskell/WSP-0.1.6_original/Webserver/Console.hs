-- | Copyright 2005 by Peter Thiemann. The console interface to server
-- administration. 
module Console where

import Char
import Control.Concurrent.MVar
import Directory
import IO
import List
import Monad

import WASH.CGI.CGI
import WASH.CGI.CGITypes
import WASH.CGI.BaseCombinators (unsafe_io)
import WASH.Utility.Auxiliary
import WASH.Utility.BulkIO
import WASH.Utility.Unique

import Compile
import qualified Deploy
import Deployment
import State

-- | Root directory of all deployments. Should be configurable.
depRoot = "/tmp/Webserver/deployments/"

-- | main entry point for the console application
console = forever $
  do st <- unsafe_io (readMVar currentState)
     standardQuery "HWS+WASH Console" $ do
       p $ text "Currently deployed applications"
       cbs <- listDeployments st
       p $ text "Select an action:"
       p $ do
	 submit cbs Console.activate (attr "value" "Activate")
	 submit cbs passivate (attr "value" "Passivate")
	 submit cbs changeOptions (attr "value" "Options")
	 makeA "" "" (text "Refresh")			    -- button?
	 submit cbs undeploy (attr "value" "Undeploy")
	 submit0 deployNew (attr "value" "Deploy new")

getNames (FL cbs) =
  loop cbs
  where
    loop [] = []
    loop (FA name hdl : rest) =
      if value hdl then name : loop rest else loop rest

changeOptions handles =
  let names = getNames handles
  in  
  standardQuery ("HWS+WASH Console/Options") $ do
    p $ text "Set options on the following deployments"
    let noInput = null names
	modifier = if noInput then attr "disabled" "disabled" else empty
    if noInput
       then p $ text "NONE"
       else ul $ mapM_ (li . text) names
    p $ text "Choose an option"
    optH <- selectBounded (Just LogOnly) empty
    p $ do
      submit optH (doChangeOptions names) (attr "value" ("SET OPTIONS") ## modifier)
      makeA "" "" (text "CANCEL")
      -- submit0 restart (attr "value" ("CANCEL"))

doChangeOptions names optH =
  let opt = value optH 
      action = unsafe_io $ Deploy.modifyOptions names (++[SessionMode opt])
  in  confirmAction ("Set option "++show opt) names action

confirmAction what names action =
  standardQuery ("HWS+WASH Console/" ++ what) $ do
    p $ text (what ++ " the following deployments")
    let noInput = null names
	modifier = if noInput then attr "disabled" "disabled" else empty
    if noInput
       then p $ text "NONE"
       else ul $ mapM_ (li . text) names
    p $ do
      submit0 action (attr "value" ("CONFIRM " ++ what) ## modifier)
      makeA "" "" (text ("CANCEL " ++ what))
      -- submit0 restart (attr "value" ("CANCEL " ++ what))

activate handles =
  let names = getNames handles
      action = unsafe_io $ Deploy.activate names
  in  confirmAction "ACTIVATE" names action

passivate handles =
  let names = getNames handles
      action = unsafe_io $ Deploy.passivate names
  in  confirmAction "PASSIVATE" names action

undeploy handles = 
  let names = getNames handles
      action = unsafe_io $ Deploy.undeploy names
  in  confirmAction "UNDEPLOY" names action

-- | Creates and fills in a new deployment descriptor, uploads all relevant
-- files into a fresh directory, attempts to compile and then load the uploaded
-- application. 
deployNew = 
  let 
    newDesc = Descriptor
      { applicationName = ""
      , applicationDescription = ""
      , applicationPath = "/"
      -- system stuff
      , deploymentDirectory = ""
      , extra_resources = []
      , mainModule = ""
      , mainSymbol = ""
      , sources = []
      , libraries = []
      , extra_libraries = []
      , packages = []
      , compilerFlags = []
      , linkerFlags = []
      }
  in deployInitUser newDesc empty

deployInitUser newDesc message =
  standardQuery "HWS+WASH Console/create new deployment" $ do
  message
  table $ do
    tr (td (text "Application Data"))
    nameF <- tr (td (text "Application Name") >> 
		 (td $ inputField (attr "value" (applicationName newDesc))))
    descF <- tr (td (text "Application Description") >>
		 (td $ inputField (attr "value" (applicationDescription newDesc))))
    pathF <- tr (td (text "Application Path") >>
		 (td $ inputField (attr "value" (applicationPath newDesc))))
    tr (td (submit (F3 nameF descF pathF) (deployInit newDesc) (attr "value" "CREATE")))

deployInit oldDesc (F3 nameF descF pathF) = do
  let name = unNonEmpty (value nameF)
      desc = unNonEmpty (value descF)
      path = unNonEmpty (value pathF)
      depDir = depRoot ++ path
      newDesc = oldDesc { applicationName = name
			, applicationDescription = desc
			, applicationPath = path
			, deploymentDirectory = depDir
			}
      bailout msg = deployInitUser newDesc (p $ text msg >> attr "style" "color: red")
  case path of
    '/':x:xs | isAlphaNum x -> do
      pathExists <- io $ do assertDirectoryExists depRoot (return ())
			    liftM (either (Left . const ()) Right) $
			      try (createDirectory depDir)
      case pathExists of
	Left _ -> 
	  bailout "Unable to create application directory. Change the path."
	Right _ ->
	  deployLoop newDesc
    _ ->
      bailout "Illegal path name."

deleteLast desc0 get set (F2 mmodF msymF) =
  let mmod = unNonEmpty (value mmodF)
      msym = unNonEmpty (value msymF)
      
      desc1 = desc0 { mainModule = mmod
		    , mainSymbol = msym
		    }
      desc2 = set desc1 (List.init $ get desc1)
  in
  deployLoop desc2

addLast desc0 get set (F2 (F2 mmodF msymF) xresF) =
  let mmod = unNonEmpty (value mmodF)
      msym = unNonEmpty (value msymF)
      xres = unNonEmpty (value xresF)
      
      desc1 = desc0 { mainModule = mmod
		    , mainSymbol = msym
		    }
      desc2 = set desc1 (get desc1 ++ [xres])
  in
  deployLoop desc2

deleteFile desc0 get set (F2 mmodF msymF) =
  let mmod = unNonEmpty (value mmodF)
      msym = unNonEmpty (value msymF)
      
      desc1 = desc0 { mainModule = mmod
		    , mainSymbol = msym
		    }
      desc2 = set desc1 (List.init $ get desc1)
			
      whichFile = deploymentDirectory desc0 ++ '/' : List.head (get desc1)
  in
  do io $ liftM (const ()) $ try $ removeFile whichFile
     deployLoop desc2

addFile desc0 get set (F2 (F2 mmodF msymF) fileF) = do
  let mmod = unNonEmpty (value mmodF)
      msym = unNonEmpty (value msymF)
      file = value fileF

      extName = fileReferenceExternalName file
      depDir = deploymentDirectory desc0
      sourceName = fileReferenceName file
      targetName = depDir ++ '/' : extName
	
  io $ do -- putStrLn ("opening " ++ sourceName)
	  src <- openFile sourceName ReadMode
	  trg <- openFile targetName WriteMode
	  rawHandleCopy src trg
	  hClose src
	  hClose trg
	  removeFile sourceName
  
  let desc1 = desc0 { mainModule = mmod
		    , mainSymbol = msym
		    }
      desc2 = set desc1 (get desc1 ++ [extName])
			
  deployLoop desc2

genericEdit add delete field desc title f2parm get set =
  do multiRow desc title get
     tr (do td empty
	    td $ do 
	      submit f2parm
		(delete desc get set)
		(attr "value" "DELETE LAST")
	      xresF <- field empty
	      submit (F2 f2parm xresF) 
		(add desc get set)
		(attr "value" "ADD"))

fileEdit x = 
  genericEdit addFile deleteFile fileInputField x
    -- eta expanded to subvert monomorphism restriction

listEdit =
  genericEdit addLast deleteLast inputField
    
deployLoop newDesc =
  standardQuery "HWS+WASH Console/install new deployment" $ do
    table $ do
      headOfDeployment newDesc
	
      tr (td (text "System parameters"))
      mmodF <- tr (td (text "Main Module") >> 
		   (td $ inputField (attr "value" (mainModule newDesc))))
      msymF <- tr (td (text "Main Symbol") >>
		   (td $ inputField (attr "value" (mainSymbol newDesc))))
      let f2parm = F2 mmodF msymF
      fileEdit newDesc "Extra resources" f2parm
	extra_resources (\r x -> r {extra_resources=x})
      fileEdit newDesc "Source files" f2parm
	sources (\r x -> r {sources=x})
      fileEdit newDesc "Library files" f2parm
	libraries (\r x -> r {libraries=x})
      listEdit newDesc "Extra libraries" f2parm
	extra_libraries (\r x -> r {extra_libraries=x})
      listEdit newDesc "Required packages" f2parm
	packages (\r x -> r {packages=x})
      listEdit newDesc "Compiler flags" f2parm
	compilerFlags (\r x -> r {compilerFlags=x})
      listEdit newDesc "Linker flags" f2parm
	linkerFlags (\r x -> r {linkerFlags=x})
      tr $ td $ do
	submit f2parm (deployNew2 newDesc) (attr "value" "FINISH")
	makeA "" "" (text "CANCEL")
	-- submit0 restart (attr "value" "CANCEL")

deployNew2 desc0 (F2 mmodF msymF) =
  let mmod = unNonEmpty (value mmodF)
      msym = unNonEmpty (value msymF)
      newDesc = desc0 { mainModule = mmod
		      , mainSymbol = msym
		      }
  in
  standardQuery "HWS+WASH Console/check new deployment" $ table $ do
    headOfDeployment newDesc
    bodyOfDeployment newDesc
    let checkSources = if null (sources newDesc)
			   then ["no source files"]
			   else []
	checkMainModule = if mainModule newDesc ++ ".hs" `elem` sources newDesc
			  then []
			  else ["main module not uploaded"]
	checkAll = checkSources ++ checkMainModule
	allOk = null checkAll
	modifier = if allOk then empty else attr "disabled" "disabled"
	oneCheck str = tr (td empty ##
			   td (text str >> attr "style" "color: red"))
    mapM_ oneCheck checkAll
    tr $ td $ do
      submit0 (deployComp newDesc) (attr "value" "DEPLOY" ## modifier)
      submit0 (deployLoop newDesc) (attr "value" "EDIT")
      makeA "" "" (text "CANCEL")
      -- submit0 restart (attr "value" "CANCEL")

-- | Compile uploaded deployment.
deployComp newDesc = do
  cr <- io $
       do compResult <- compileServlet newDesc
	  case compResult of
	    Left errorMessage ->
	      return $ Left errorMessage
	    Right newObj ->
	      do st <- takeMVar currentState
		 let deployments = deploymentState st
		     newDeployments = deployments 
		       { desc = newDesc : desc deployments
		       , objs = newObj  : objs deployments
		       }
		     newSt = st { deploymentState = newDeployments }
		 putMVar currentState newSt
		 writeFile (deploymentDirectory newDesc ++ "/DESCRIPTOR")
			   (show newDesc)
		 return $ Right ()
  case cr of
    Left errorMessage ->
      deployCompFailed newDesc errorMessage
    Right dobj ->
      deployCompSucceeded newDesc dobj

deployCompFailed newDesc errorMessage =
  standardQuery "HWS+WASH Compilation Failure" $ do
    p $ do text "Compilation of application "
	   text (applicationName newDesc)
	   text " failed with the following error message."
    pre $ text $ unlines errorMessage
    submit0 (deployLoop newDesc) (attr "value" "CONTINUE")

deployCompSucceeded newDesc newObj = 
  standardQuery "HWS+WASH Compilation Success" $ do
    p $ do text "Compilation of application "
	   text (applicationName newDesc)
	   text " succeeded."
    p $ text "Application loaded. Status: passive."
    makeA "" "" (text "CONTINUE")
    -- submit0 restart (attr "value" "CONTINUE")

singleRow newDesc label get =
  tr (td (text label) >> (td (tt (text (get newDesc)))))

multiRow newDesc label get =
  tr (td (text label) >> (td $ table $ tr $ stringListToCells (get newDesc)))

headOfDeployment newDesc = do 
      tr (td (text "Application"))
      singleRow newDesc "Application Name" applicationName
      singleRow newDesc "Description" applicationDescription
      singleRow newDesc "Path" applicationPath

bodyOfDeployment newDesc = do
      tr (td (text "System parameters"))
      singleRow newDesc "Main Module" mainModule
      singleRow newDesc "Main Symbol" mainSymbol
      multiRow  newDesc "Extra resources" extra_resources
      multiRow  newDesc "Source files" sources
      multiRow  newDesc "Library files"	libraries
      multiRow  newDesc "Extra libraries" extra_libraries
      multiRow  newDesc "Required packages" packages
      multiRow  newDesc "Compiler flags" compilerFlags
      multiRow  newDesc "Linker flags" linkerFlags

tableOfDeployment newDesc =
  table $ do
    headOfDeployment newDesc
    bodyOfDeployment newDesc
 

stringListToCells :: [String] -> WithHTML x CGI ()
stringListToCells =
  mapM_ (td . tt . text)


listDeployments st =
  let deployments = deploymentState st in
     table $ do
       attr "border" "1"
       tr $ do
	 th $ text ""
	 th $ text "Name"
	 th $ text "Path"
	 th $ text "Module"
	 th $ text "Kind"
	 th $ text "Status"
	 th $ text "Options"
       l <- mapM (showDeployedObject (desc deployments)) (objs deployments)
       return $ FL l
     
showDeployedObject descriptors dobj =
  let path = objectPath dobj in
  tr $ do
    cb <- td (checkboxInputField empty)
    td (text $ objectName dobj)
    td $ a (attr "href" path ## tt (text path))
    td (text $ objectMod dobj)
    td (text $ showKind $ objectSystem dobj)
    td (text $ showStatus $ objectActive dobj)
    td (text $ showOptions $ objectOptions dobj)
    return (FA (objectName dobj) cb)

showKind True = "System"
showKind False = "User"

showStatus True = "Active"
showStatus False = "Passive"

showOptions xs = 
  g LogOnly xs
  where
    g def [] = show def
    g def (SessionMode ssm : xs) = g ssm xs
    g def (_ : opts) = g def opts

deploymentCandidates :: IO [Descriptor]
deploymentCandidates = do
  return []

