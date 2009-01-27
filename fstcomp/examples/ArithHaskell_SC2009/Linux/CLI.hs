module CLI where
{ import System.Console.SimpleLineEditor as SLE;
 
  loop :: IO ();
  loop
    = do { l <- SLE.getLineEdited "> ";
           case l of
               { Nothing -> return ();
                 Just "" -> return ();
                 Just ll
                   -> do { Control.Exception.catch (putStrLn $ parseAndEval ll) print;
                           loop}}}
    where { parseAndEval str
              = case parse parser "" str of
                    { Left pe -> show pe;
                      Right res -> show $ evalExp res};
            parser
              = do { e <- expr;
                     eof;
                     return e}};
   
  main :: IO ();
  main
    = do { SLE.initialise;
           putStrLn "Simple command line interpreter. Exit with empty input.";
           loop;
           SLE.restore}}
