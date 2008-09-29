module CLI where
{ import Control.Exception (catch);
  import System.Console.SimpleLineEditor as SLE;
  import System.IO;
  import Text.ParserCombinators.Parsec;
  import Text.ParserCombinators.Parsec.Expr;
  import Text.ParserCombinators.Parsec.Language;
  import qualified Text.ParserCombinators.Parsec.Token as PT;
  
  import Arith;
  
  lexer :: PT.TokenParser ();
  lexer
    = PT.makeTokenParser
        (haskellDef{reservedNames = ["let", "in", "ite", "true", "false"],
        						-- TODO MRO: - bei BinOps ODER UnOps
                    reservedOpNames = ["+", "-", "*", "/", "&&", "||", "#", "!", "\\", "=", "->"]});
  reserved = PT.reserved lexer;
  reservedOp = PT.reservedOp lexer;
  parens = PT.parens lexer;
  semi = PT.semi lexer;
  identifier = PT.identifier lexer;
  naturalOrFloat = PT.naturalOrFloat lexer;
  stringLiteral = PT.stringLiteral lexer;
  
  table
  -- TODO MRO: && und ||
    = [[op "*" Mul AssocLeft, op "/" Div AssocLeft],
       [op "+" Add AssocLeft, op "-" Sub AssocLeft],
       [op "&&" And AssocLeft], [op "||" Or AssocLeft]]
    where { op s f assoc
              = Infix
                  (do { reservedOp s;
                        return (Binary f)}
                     <?> "operator")
                  assoc};
   
  expr :: Parser (Exp TypedVal);
  expr = buildExpressionParser table factor <?> "expression";
  
  -- TODO MRO: färben
  factor :: Parser (Exp TypedVal);
  factor
    =   do { reservedOp "-";
           e <- expr;
           return (Unary Neg e)}
        <|>
        do { reservedOp "#";
             e <- expr;
             return (Unary Recip e)}
        <|>
        do { reservedOp "!";
             e <- expr;
             return (Unary Not e)}
        <|> letExpr
        <|> lamExpr
        <|> iteExpr
        <|> appExpr
        <?> "simple expression";
   
  letExpr :: Parser (Exp TypedVal);
  letExpr
    = do { reserved "let";
           ds <- sepBy decl semi;
           reserved "in";
           e <- expr;
           return (Let ds e)}
        <?> "let expression";
   
  decl :: Parser (String, Exp TypedVal);
  decl
    = do { x <- identifier;
           reservedOp "=";
           e <- expr;
           return (x, e)}
        <?> "variable binding";
   
  lamExpr :: Parser (Exp TypedVal);
  lamExpr
    = do { reservedOp "\\";
           x <- identifier;
           reservedOp "->";
           e <- expr;
           return (Lam x e)}
        <?> "lambda expression";
   
  iteExpr :: Parser (Exp TypedVal);
  iteExpr
    = do { reserved "ite";
           cond <- appArg;
           t <- appArg;
           e <- appArg;
           return (ITE cond t e)}
        <?> "conditional expression";
   
  appExpr :: Parser (Exp TypedVal);
  appExpr
    = do { es <- many1 appArg;
           return (foldl1 App es)}
        <?> "application";
   
  appArg :: Parser (Exp TypedVal);
  appArg
    = (do { i <- identifier;
            return (Var i)}
         <?> "identifier")
        <|> parens expr
        <|> number
        <|> stringConst
        <|> boolConst
        <?> "argument expression";
   
  number :: Parser (Exp TypedVal);
  number
    = do { nof <- naturalOrFloat;
           case nof of
               { Right d -> return (Const (TVDouble d));
                 Left n -> return (Const (TVDouble (fromIntegral n)))}}
        <?> "number";
   
  stringConst :: Parser (Exp TypedVal);
  stringConst
    = do { s <- stringLiteral;
           return (Const (TVString s))}
        <?> "string literal";
        
  boolConst :: Parser (Exp TypedVal);
  boolConst
    = do { reserved "true";
           return (Const (TVBool True))}
      <|>
      do { reserved "false";
           return (Const (TVBool False))}
      <?> "bool constant";
   
  loop :: IO ();
  loop
    = do { l <- SLE.getLineEdited "> ";
    	   putStr "> ";
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
    = do { SLE.initialise;
    	   hSetBuffering stdout NoBuffering;
           putStrLn "Simple command line interpreter. Exit with empty input.";
           loop;
           SLE.restore}}
