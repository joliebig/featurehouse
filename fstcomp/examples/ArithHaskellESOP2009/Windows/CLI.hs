module CLI where
{ import System.IO;
  
  loop :: IO ();
  loop
    = do { putStr "> ";
           l' <- getLine;
           let { l = Just l'};
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
    = do { hSetBuffering stdout NoBuffering;
           putStrLn "Simple command line interpreter. Exit with empty input.";
           loop
           }}
