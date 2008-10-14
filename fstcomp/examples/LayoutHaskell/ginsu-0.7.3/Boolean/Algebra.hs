{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}
-- arch-tag: 8aa47e06-b867-41c7-8127-4d0172706024

-- | This is the main module of the Boolean hierachy and provides a class which
-- abstracts common operations on boolean algebras.  note, we redefine some
-- prelude functions, but the new definitons mean the same thing for Bool so it
-- will not hurt existing code.
--
-- to use properly:
--
-- > import Boolean.Algebra
-- > import Prelude hiding((&&),(||),not,and,or,any,all)
--


module Boolean.Algebra(
    SemiBooleanAlgebra(..),
    BooleanAlgebra(..),
    and1, or1, fromBool
) where

import Prelude hiding((&&),(||),not,and,or,any,all)
import Monad

infixr 3 &&
infixr 2 ||
infixr 2 `xor`

--------------
-- The class
--------------

-- | This class is mainly for syntax re-use, there are many types which are very similar
-- to boolean algebras, but do not have suitable distinguished values to choose for true
-- and false.
--
-- '&&' and '||' should be strict only in their first argument, and return one
-- of their arguments if possible.

class SemiBooleanAlgebra a where
    (&&) :: a -> a -> a
    (||) :: a -> a -> a
    --a && b = not (not a || not b)
    --a || b = not (not a && not b)




-- | This is the main class, providing all the operations one would expect on
-- a boolean algebra.
class SemiBooleanAlgebra a => BooleanAlgebra a where
    true :: a
    false :: a
    not :: a -> a
    -- the following are in case there is a more efficient implementation
    -- than the default
    xor :: a -> a -> a
    and :: [a] -> a
    or :: [a] -> a
    any :: (x -> a) -> [x] -> a
    all :: (x -> a) -> [x] -> a

    any f xs = or (map f xs)
    all f xs = and (map f xs)
    and xs  = foldr (&&) true xs
    or xs  = foldr (||) false xs
    xor a b = (a && not b) || (b && not a)
    true = not false
    false = not true
    not x = xor true x

------------------------
-- some useful functions
------------------------

-- | this behaves identically to 'and' but requires there be at least one item in
-- the list and has a more general type.
and1 :: SemiBooleanAlgebra a => [a] -> a
and1 xs = foldr1 (&&) xs


-- | this behaves identically to 'or' but requires there be at least one item in
-- the list and has a more general type.
or1 :: SemiBooleanAlgebra a => [a] -> a
or1 xs = foldr1 (||) xs

-- | convert a 'Bool' into an arbitrary membor of BooleanAlgebra.
fromBool :: BooleanAlgebra a => Bool -> a
fromBool True = true
fromBool False = false


------------
-- Instances
------------

instance SemiBooleanAlgebra (Maybe a) where
    (Just _) && x = x
    x && _ = x
    Nothing || x = x
    x || _ = x

-- | This behaves similarly to the Maybe instance, treating the empty list as
-- false, and any other list as true. Both routines will return one of their
-- arguments unmodified after evaluating their first argument.

instance SemiBooleanAlgebra [a] where
    (_:_) && x = x
    x && _ = x
    [] || x = x
    x || _ = x

-- | For the purposes of this instance 'Left' is considered false and 'Right'
-- true.

instance SemiBooleanAlgebra (Either a b) where
    (Right _) && x = x
    x && _ = x
    (Left _) || x = x
    x || _ = x

instance SemiBooleanAlgebra Bool where
    False && _ = False
    True && x = x
    True || _ = True
    False || x = x

instance BooleanAlgebra Bool where
    true = True
    false = False
    not True = False
    not False = True
    xor True False = True
    xor False True = True
    xor _ _ = False


instance SemiBooleanAlgebra a => SemiBooleanAlgebra (x -> a) where
    f && g = \x -> f x && g x
    f || g = \x -> f x || g x


-- | This is the space of predicates
instance BooleanAlgebra a => BooleanAlgebra (x -> a) where
    true = \_ -> true
    false = \_ -> false
    not f = \x -> not (f x)
    xor f g = \x -> f x `xor` g x


-- TODO need more instances for tuples
instance (SemiBooleanAlgebra a, SemiBooleanAlgebra b) => SemiBooleanAlgebra (a,b) where
    (x,y) && (x',y') = (x && x', y && y')
    (x,y) || (x',y') = (x || x', y || y')

instance (BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (a,b) where
    not (x,y)  = (not x, not y)
    true = (true,true)
    false = (false,false)


-- | zero is false, any other value is true
instance SemiBooleanAlgebra Int where
    0 && _ = 0
    _ && b = b
    0 || b = b
    a || _ = a

instance BooleanAlgebra Int where
    false = 0
    true = 1
    not 0 = 1
    not _ = 0
    xor 0 b = b
    xor _ b = not b

instance SemiBooleanAlgebra Integer where
    0 && _ = 0
    _ && b = b
    0 || b = b
    a || _ = a

instance BooleanAlgebra Integer where
    false = 0
    true = 1
    not 0 = 1
    not _ = 0
    xor 0 b = b
    xor _ b = not b


instance (Monad m, SemiBooleanAlgebra a) => SemiBooleanAlgebra (m a)  where
    (&&) = liftM2 (&&)
    (||) = liftM2 (||)

instance (Monad m, SemiBooleanAlgebra (m a), BooleanAlgebra a) => BooleanAlgebra (m a)  where
    xor = liftM2 xor
    not = liftM not
    true = return true
    false = return false
    and xs = sequence xs >>= return . and
    or xs = sequence xs >>= return . or
    any f xs = mapM f xs >>= return . or
    all f xs = mapM f xs >>= return . and


