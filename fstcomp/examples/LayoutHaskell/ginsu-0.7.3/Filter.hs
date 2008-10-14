module Filter(
    Filter,
    BasicFilter(..),
    parseFilter,
    showFilter,
    filterAp
    ) where

import Atom
import Boolean.Algebra
import Boolean.Boolean
import Char
import PackedString
import Prelude hiding((&&),(||),not,and,or,any,all)
import Gale.Puff
import Regex
import Text.ParserCombinators.Parsec

------------------------
-- Filter implementation
------------------------

type Filter = Boolean BasicFilter

data BasicFilter =
    FilterAuthor String
    | FilterCategory Category
    | FilterRegex Atom Rx
    | FilterSearchAll Rx
    | FilterFlag Atom
    | FilterMark {-# UNPACK #-} !Char
    | FilterAlias Atom


parseFlag :: Parser BasicFilter
parseFlag = do
    char '?'
    n <- many1 (satisfy isAlpha)
    spaces
    return $ FilterFlag (toAtom n)


parseTrue = try $ do
    FilterFlag n <- parseFlag
    if n == toAtom "true" then return () else fail "true"
parseFalse = try $ do
    FilterFlag n <- parseFlag
    if n == toAtom "false" then return () else fail "false"


parseBasic = parseMark <|> parseFlag <|> parseSlash <|> parseTwiddle <|> parseAlias where
    parseSlash = do
        char '/'
        s <- string
        spaces
        s <- compileRx s
        return $ FilterSearchAll s
    parseTwiddle = do
        char '~'
        f <- satisfy isAlpha
	option ':' (char ':')
        s <- string
        spaces
        z f s
    parseMark = do
        char '"'
        c <- satisfy isAlphaNum
        spaces
        return $ FilterMark c

    z 'a' s = return $ FilterAuthor s
    z 'c' s = return $ FilterCategory (catParseNew s)
    z 't' s = return $ FilterCategory (catParseNew s) -- TODO
    z 's' s = compileRx s >>= return . FilterRegex f_messageSender
    z 'k' s = compileRx s >>= return . FilterRegex f_messageKeyword
    z 'b' s = compileRx s >>= return . (FilterRegex f_messageBody)
    z x _ = fail $ "unknown filter type: ~" ++ [x]
    parseAlias = do
        n <- many1 (satisfy isAlphaNum)
        return $ FilterAlias (toAtom n)
    string = qstring <|> many1 (noneOf " \t\n':;|")
    qstring = between (char '\'') (char '\'') $ many ((char '\'' >> char '\'' >> return '\'') <|> noneOf "'")


parseFilter :: Monad m => String -> m Filter
parseFilter s = case parse (between spaces eof pb)  "" s  of
        Left e -> fail (show e)
        Right x -> return x
    where
    pb = parseBoolean' spaces parseTrue parseFalse parseBasic

{-
parseFilter :: Monad m => String -> m Filter
parseFilter = parser (between spaces eof rmp) where
    rmp = do
	fa <- mp
	r <- rmp'
	case r of
	    [] -> return fa
	    _ -> return $ FilterOr (fa:r)
    rmp' = (do
	char ';'
	spaces
	v <- mp
	r <- rmp'
	return (v:r)) <|> return []
    mp = many1 fp >>= \v -> case v of
	    [x] -> return x
	    xs -> return $ FilterAnd xs
    fp = nf <|> flt <|> pr <|> bool <|> do s <- (token string); liftM (FilterRegex f_messageBody) (compileRx s)
    pr = do
	char '('
	spaces
	v <- rmp
	char ')'
	spaces
	return v
    nf = char '!' >> liftM FilterNot fp
    string = qstring <|> many1 (noneOf " \t\n~'!();%")
    qstring = between (char '\'') (char '\'') $ many ((char '\'' >> char '\'' >> return '\'') <|> noneOf "'")
    bool = do
	char '%'
	c <- sat isAlpha
	spaces
	return $ b c
    flt = do
	char '~'
	c <- sat isAlpha
	option ':' (char ':')
	s <- string
	spaces
	z c s
    z 'a' s = return $ FilterAuthor s
    z 'c' s = return $ FilterCategory (catParseNew s)
    z 't' s = return $ FilterCategory (catParseNew s) -- TODO
    z 's' s = compileRx s >>= return . FilterRegex f_messageSender
    z 'k' s = compileRx s >>= return . FilterRegex f_messageKeyword
    z _ s = compileRx s >>= return . (FilterRegex f_messageBody)
    b 'p' = FilterPrivate
    b 'P' = FilterNot FilterPrivate
    b _ = FilterTrue
-}

showFilter f = showBoolean showBasicFilter f

showBasicFilter :: BasicFilter -> String
showBasicFilter (FilterAuthor a) = "~a:" ++  a
showBasicFilter (FilterCategory c) = "~c:" ++ catShowNew c
showBasicFilter (FilterRegex fn s)
    | fn == f_messageKeyword = "~k:" ++ show s
    | fn == f_messageSender = "~s:" ++ show s
    | fn == f_messageBody = "~b:" ++ show s
    | otherwise = "~UNKNOWN"
showBasicFilter (FilterSearchAll s) = "/" ++ show s
showBasicFilter (FilterFlag s) = "?" ++ fromAtom s
showBasicFilter (FilterAlias x) = fromAtom x
showBasicFilter (FilterMark m) = '"':m:""
--showBasicFilter (FilterAny fn r) = "~A:" ++ toString fn ++ ":" ++ show r
--showBasicFilter (FilterRegex fn r) = "~R:" ++ toString fn ++ ":" ++ show r
--showBasicFilter (FilterNot f) = "!" ++ showFilter f
--showBasicFilter (FilterAnd fs) = concat (intersperse " " (map showFilter fs))
--showBasicFilter (FilterOr fs) = "(" ++ (concat $ intersperse " ; " (map showFilter fs)) ++ ")"
--showBasicFilter FilterPrivate = "%p"
--showBasicFilter (FilterNot FilterPrivate) = "%P"
--showBasicFilter FilterTrue = "%t"
--showBasicFilter (FilterNot FilterTrue) = "%f"

{-
applyFilterStack :: [Filter] -> [(Int, Puff)] -> [(Int, Puff)]
applyFilterStack fs = concatMap f where
    f a@(_,p) | all (flip (evaluate filterAp) p) fs = [a]
    f _ = []
-}

siglookup c (Unverifyable (Key n _):_) | c == n = True
siglookup c (Signed (Key n _):_) | c == n = True
siglookup c (_:xs) = siglookup c xs
siglookup _ [] = False

isSigned Signed {} = True
isSigned Unverifyable {} = True
isSigned _ = False

isEncrypted Encrypted {} = True
isEncrypted _ = False

{-
data FilterEnv = FilterEnv {
    feMarkLookup :: Char -> Filter,
    feAliasLookup :: Atom -> Filter,
    feIsPrivate :: Category -> Bool,
    feIsPublic :: Category -> Bool
    }
-}

data FilterEnv

filterAp :: Filter -> Puff -> Bool
filterAp f p = evaluate (filterAp' undefined p) f

filterAp' :: FilterEnv -> Puff -> BasicFilter ->  Bool
filterAp' _ p (FilterAuthor c)  = siglookup c (signature p)
filterAp' _ p (FilterCategory c) = any (`subCategory` c) (cats p)
filterAp' _ p (FilterRegex fn re) = any (matchRx re) (getFragmentStrings p fn)
filterAp' _ p (FilterSearchAll re)  = any (matchRx re) (concatMap (getFragmentForceStrings p) [f_messageBody, f_messageKeyword, f_messageSender, f_idTime] ++ cs ++ a)  where
    cs = map (packString . catShowNew) (cats p)
    a = [packString $ getAuthor p]
filterAp' _ p (FilterFlag f)
    | f == toAtom "signed" = any isSigned (signature p)
    | f == toAtom "encrypted" = any isEncrypted (signature p)
    | f == toAtom "true" = true
    | f == toAtom "false" = false
    | otherwise = false
filterAp' _ _ (FilterMark _)  = False
filterAp' _ _ (FilterAlias _) = False

--filterAp (FilterAnd fs) p = all (`filterAp` p) fs
--filterAp (FilterOr fs) p = any (`filterAp` p) fs
--filterAp (FilterAny fn re) p = any (matchRx re) (getFragmentForceStrings p fn)
--filterAp (FilterNot f) p = not $ filterAp f p
--filterAp FilterTrue _ = True
--filterAp FilterPrivate _ = False

--showFilter _ = ""


