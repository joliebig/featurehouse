-- | 
-- Maintainer: Stephan Friedrichs
--
--   An efficent implementation of min-priority heaps
--   based on the leftist-heaps from Chris Okasakis book \"/Purely Functional Data
--   Structures/\", Cambridge University Press, chapter 3.1, 1998.
module Data.Heap (
	-- * Heap type
	Heap,
	-- * Query
	null, isEmpty, size, findMin,
	-- * Construction
	empty, singleton,
	insert, deleteMin, deleteFindMin,
	removeWhile,
	-- * Combine
	union, unions,
	-- * Conversion
	-- ** List
	fromList, toList, elems,
	-- ** Ordered list
	fromAscList, toAscList,
	-- * Debugging
	check
) where

import Data.List (foldl')
import Data.Monoid
import Data.Ord
import Prelude hiding (null)

-- | The 'Heap' type.
data Heap a
	= Empty
	| Tree Int a (Heap a) (Heap a)

instance (Show a) => Show (Heap a) where
	show h = "fromList " ++ (show.toList) h

instance Ord a => Eq (Heap a) where
	a == b = EQ == compare a b

instance Ord a => Ord (Heap a) where
	compare = comparing toAscList

instance Ord a => Monoid (Heap a) where
	mempty  = empty
	mappend = union
	mconcat = unions

-- | /O(1)/. Is the 'Heap' empty?
null :: Heap a -> Bool
null Empty = True
null _     = False

-- | /O(1)/. Is the 'Heap' empty?
isEmpty :: Heap a -> Bool
isEmpty = null

-- | /O(1)/. Calculate the rank of a 'Heap'.
rank :: Heap a -> Int
rank Empty = 0
rank (Tree r _ _ _) = r

-- | /O(n)/. The number of elements in the 'Heap'.
size :: Heap a -> Int
size Empty = 0
size (Tree _ _ a b) = 1 + size a + size b

-- | /O(1)/. Finds the minimum of the 'Heap'.
findMin :: Ord a => Heap a -> a
findMin = fst.deleteFindMin 

-- | /O(1)/. Constructs an empty 'Heap'.
empty :: Heap a
empty = Empty

-- | /O(1)/. Create a singleton 'Heap'.
singleton :: a -> Heap a
singleton x = Tree 1 x empty empty

-- | /O(log n)/. Insert an element in the 'Heap'.
insert :: Ord a => a -> Heap a -> Heap a
insert x h = union h (singleton x)

-- | /O(log n)/. Delete the minimum from the 'Heap'.
deleteMin :: Ord a => Heap a -> Heap a
deleteMin = snd.deleteFindMin

-- | /O(log n)/. Find the minimum and delete it from the 'Heap'.
deleteFindMin :: Ord a => Heap a -> (a, Heap a)
deleteFindMin Empty = (error "Heap is empty", Empty)
deleteFindMin (Tree _ x a b) = (x, union a b)

-- | Removes and returns results as long as the supplied function returns 'Just'.
removeWhile :: Ord a => (a -> Maybe b) -> Heap a -> (Heap a, [b])
removeWhile f heap
	| null heap = (heap, [])
	| otherwise = let (ak, nheap) = deleteFindMin heap in case f ak of
		Nothing  -> (heap, [])
		Just rem -> let (nnheap, rest) = removeWhile f nheap in (nnheap, rem:rest)

-- | /O(log max(n, m))/. The union of two 'Heap's.
union :: Ord a => Heap a -> Heap a -> Heap a
union h Empty = h
union Empty h = h
union heap1@(Tree _ x l1 r1) heap2@(Tree _ y l2 r2) = if x <= y
	then makeT x l1 (union r1 heap2) -- keep smallest number on top and merge the other
	else makeT y l2 (union r2 heap1) -- heap into the right branch, it's shorter

-- | Combines a value x and two Heaps to one Heap. Therefore, x has to be less or
--   equal the minima of both 'Heap' parameters. The precondition is not checked.
makeT :: a -> Heap a -> Heap a -> Heap a
makeT x a b = if ra > rb
	then Tree (rb + 1) x a b
	else Tree (ra + 1) x b a
	where ra = rank a
	      rb = rank b

-- | Builds the union over all given 'Heap's.
unions :: Ord a => [Heap a] -> Heap a
unions = foldl' union empty

-- | Builds a 'Heap' from the given elements. If you have a sorted list, use 'fromAscList', which
--   is much more efficient.
fromList :: Ord a => [a] -> Heap a
fromList = unions.(map singleton)

-- | /O(n)/. Lists elements of the 'Heap' in no specific order.
toList :: Heap a -> [a]
toList Empty = []
toList (Tree _ x a b) = x : toList a ++ toList b

-- | /O(n)/. Lists elements of the 'Heap' in no specific order.
elems :: Heap a -> [a]
elems = toList

-- | /O(n)/. Creates a 'Heap' from an ascending list. /The precondition is not checked./
fromAscList :: Ord a => [a] -> Heap a
fromAscList []     = Empty
fromAscList (x:xs) = Tree 1 x (fromAscList xs) Empty

-- | /O(n)/. Lists elements of the 'Heap' in ascending order.
toAscList :: Ord a => Heap a -> [a]
toAscList Empty          = []
toAscList (Tree _ x a b) = x : mergeLists (toAscList a) (toAscList b)
	where	mergeLists [] ys = ys
		mergeLists xs [] = xs
		mergeLists xs@(x:xs') ys@(y:ys') = if x < y
	      		then x : mergeLists xs' ys
			else y : mergeLists xs ys'

-- | Sanity checks for debugging. This includes checking the ranks and the heap- and leftist
--   (the left rank is at least the right rank) properties.
check :: Ord a => Heap a -> Bool
check Empty = True
check (Tree r x left right) = (isEmpty left || findMin left >= x)
	&& (isEmpty right || findMin right >= x)
	&& r == 1 + rightRank
	&& leftRank >= rightRank
	&& check left
	&& check right
	where leftRank  = rank left
	      rightRank = rank right

