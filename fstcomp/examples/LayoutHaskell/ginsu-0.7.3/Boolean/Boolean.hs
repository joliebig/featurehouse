{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- arch-tag: 15d3cb07-cddc-4f7b-8fff-ca7132a88d7e

-- | This module provides a data constructor which lifts any type into a
-- boolean algebra and some operations on said lifted type.
--

module Boolean.Boolean(
    Boolean(..),
    simplifyBoolean,
    showBoolean,
    evaluate,
    evaluateM,
    parseBoolean,
    parseBoolean',
    dropBoolean
) where

import Boolean.Algebra
import Prelude hiding((&&),(||),not,and,or,any,all)
import qualified Prelude
import Monad
import List hiding(and,or)
import Text.ParserCombinators.Parsec



----------------
-- the data type
----------------

-- true is BoolAnd []
-- false is BoolOr []
data Boolean a =
    BoolNot (Boolean a)
    | BoolAnd [Boolean a]
    | BoolOr [Boolean a]
    | BoolJust a
    deriving( Eq, Ord, Show, Read)

instance Functor Boolean where
    fmap f (BoolNot x) = BoolNot (fmap f x)
    fmap f (BoolAnd xs) = BoolAnd (map (fmap f) xs)
    fmap f (BoolOr xs) = BoolOr (map (fmap f) xs)
    fmap f (BoolJust x) = BoolJust (f x)


instance Monad Boolean where
    --a >> b = a && b
    a >>= f = dropBoolean (fmap f a)
    return x = BoolJust x
    fail _ = false

instance MonadPlus Boolean where
    a `mplus` b = a || b
    mzero = false

showBoolean :: (a -> String) -> Boolean a -> String
showBoolean f (BoolNot b) = '!':showBoolean f b
showBoolean f (BoolJust x) = f x
showBoolean _ (BoolAnd []) = "true"
showBoolean _ (BoolOr []) = "false"
--show (BoolAnd [x]) = show x
--show (BoolOr [x]) = show x
showBoolean f (BoolAnd xs) = "(" ++ unwords (map (showBoolean f) xs) ++ ")"
showBoolean f (BoolOr xs) = "(" ++ concat (intersperse " ; " (map (showBoolean f) xs)) ++ ")"


-- | very safe simplification routine. This will never duplicate terms
-- or change the order terms occur in the formula, so is safe to use even
-- when bottom is present.
--
-- what it does is:
--
-- * removes double negatives
--
-- * evaluates constant terms
--
-- * removes manifest tautologies
--
-- * flattens and of and, or of or
--
-- * flattens single element terms
--
-- if the first argument is true, it also uses de morgan's laws to ensure
-- BoolNot may only occur as the parent of a BoolJust. This may allow
-- additional flattening and simplification, but may increase the number of
-- negations performed and change the ratio between ands and ors done. It does
-- not affect term order either way.


simplifyBoolean :: Bool -> Boolean a -> Boolean a
simplifyBoolean demorgan x = simplifyBoolean' x where
    simplifyBoolean' x@(BoolAnd []) = x
    simplifyBoolean' x@(BoolOr []) = x
    simplifyBoolean' (BoolAnd [x]) = simplifyBoolean' x
    simplifyBoolean' (BoolOr [x]) = simplifyBoolean' x
    simplifyBoolean' x@(BoolJust _) = x
    simplifyBoolean' (BoolNot z)
        | BoolNot y <- x = simplifyBoolean' y
        | BoolAnd [] <- x = BoolOr []
        | BoolOr [] <- x = BoolAnd []
        | demorgan, BoolAnd xs <- x = simplifyBoolean' $ BoolOr (map BoolNot xs)
        | demorgan, BoolOr xs <- x = simplifyBoolean' $ BoolAnd (map BoolNot xs)
        | otherwise = BoolNot x
        where x = (simplifyBoolean' z)
    simplifyBoolean' (BoolAnd xs)
        | [x] <- xs' = x
        | Prelude.any isFalse xs' = false
        | otherwise = BoolAnd xs'
        where
            xs' = concat $ f [] (dropWhile isTrue (map simplifyBoolean' xs))
            f xs [] = reverse xs
            f z (BoolAnd x:xs) =  f (x:z) xs
            f z (x:xs) = f ([x]:z) xs
    simplifyBoolean' (BoolOr xs)
        | [x] <- xs' = x
        | Prelude.any isTrue xs' = true
        | otherwise = BoolOr xs'
        where
            xs' = concat $ f [] (dropWhile isFalse (map simplifyBoolean' xs))
            f xs [] = reverse xs
            f z (BoolOr x:xs) =  f (x:z) xs
            f z (x:xs) = f ([x]:z) xs

-- these only account for trivial truths
isTrue (BoolAnd []) = True
isTrue _ = False
isFalse (BoolOr []) = True
isFalse _ = False


-- | perform the Boolean actions on the underlying type, flattening out the
-- Boolean wrapper. This is useful for things like wrapping an expensive to
-- compute predicate for the purposes of optimization.

dropBoolean :: BooleanAlgebra a => Boolean a -> a
dropBoolean (BoolNot x) = not (dropBoolean x)
dropBoolean (BoolOr xs) = or (map dropBoolean xs)
dropBoolean (BoolAnd xs) = and (map dropBoolean xs)
dropBoolean (BoolJust x) = x


-- | evalute Boolean given a function to evaluate its primitives

evaluate :: BooleanAlgebra r => (a -> r) -> Boolean a -> r
evaluate f x = dropBoolean (fmap f x)

-- | evalute Boolean given a monadic function to evaluate its primitives
evaluateM :: (Monad m, BooleanAlgebra r) => (a -> m r) -> Boolean a -> m r
evaluateM f (BoolJust x) = f x
evaluateM f (BoolNot x) = evaluateM f x >>= return . not
evaluateM f (BoolAnd xs) = mapM (evaluateM f) xs >>= return . and
evaluateM f (BoolOr xs) = mapM (evaluateM f) xs >>= return . or


-- | A parsec routine to parse a 'Boolean a' given a parser for 'a'.
-- The format is the same as produced by 'showBoolean':
--
-- > a ; b  - a or b
-- > a b    - a and b
-- > !a     - not a
-- > ( a )  - a
--
-- The parser passed as an argument should eat all whitespace after it matches and
-- ensure its syntax does not conflict with the Boolean syntax.

parseBoolean' ::
       Parser s -- ^ parser for whitespace
    -> Parser t -- ^ parser for true
    -> Parser f -- ^ parser for false
    -> Parser a  -- ^ parser for a
    -> Parser (Boolean a) -- ^ parser for Boolean a
parseBoolean' spaces t f pa = disj where
    disj = fmap boolOr (sepBy1 conj (char ';' >> spaces)) <?> "disjunction"
    conj = fmap boolAnd (many1 item) <?> "conjunction"
    item = do
        n <- option id (char '!' >> spaces >> return BoolNot)
        v <- (parened <|> (t >> return true) <|> (f >> return false) <|> fmap BoolJust pa)
        spaces
        return $ n v
    parened = between (char '(' >> spaces) (char ')' >> spaces) disj <?> "parenthesis"
    boolOr [x] = x
    boolOr xs = BoolOr xs
    boolAnd [x] = x
    boolAnd xs = BoolAnd xs

-- | specialized version of parser which understands 'true', 'false' and the
-- normal whitespace characters.

parseBoolean = parseBoolean' spaces (try t >> spaces) (try f >> spaces) where
    t = (string "true" >> notFollowedBy alphaNum)  <?> "true"
    f = (string "false" >> notFollowedBy alphaNum) <?> "false"


instance SemiBooleanAlgebra (Boolean a) where
    x && y = BoolAnd [x,y]
    x || y = BoolOr [x,y]

instance BooleanAlgebra (Boolean a) where
    not x = BoolNot x
    and xs = BoolAnd xs
    or xs = BoolOr xs
    true = BoolAnd []
    false = BoolOr []

