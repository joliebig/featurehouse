{-# LANGUAGE ExistentialQuantification #-}
-- | Maintainer: Stephan Friedrichs, Henning Guenther
module Network.AdHoc.MessageID
	(MessageID
	,MessageIDGenerator
	,counter
	,scrambler
	,hasher
	) where

import Data.List as List
import qualified Data.Digest.SHA512 as SHA512
import qualified Codec.Binary.Base64 as Base64
import Codec.Utils
import System.Random

-- | This is just a 'String'.
type MessageID = String

-- | Generate an infinite(!) stream of 'MessageID's.
type MessageIDGenerator = RandomGen g => g -> [MessageID]

-- | Simplest possible generator type: counts from 0 upwards. Very insecure.
counter :: MessageIDGenerator
counter _ = fmap show [0..]

-- | Generates a stream of 'MessageID's by shuffling a sorted stream couting from 0 upwards (@[0..]@).
--   @scrambler n g@ would randomly remove the i-th element (where i is a random number between 0 and n)
--   from the sorted list and place it at the head of the result list. It deals with the following
--   elements in the same way.
scrambler :: Int -> MessageIDGenerator
scrambler offset gen = fmap show $ scramble gen offset [0..]

-- | Generates a stream of IDs by hashing an up-counting number and a random number with SHA512.
hasher :: MessageIDGenerator
hasher gen = fmap ((Base64.encode).(SHA512.hash)) (zipWith (++) (fmap (toOctets 256) [0..]) (fmap toTwosComp (randoms gen::[Int])))

-- | Scrambles an infinite(!) list. The result will be a permutation of the original list.
--   The higher the offset (second argument), the further elements can occur from their
--   original indices.
scramble :: RandomGen g => g -> Int -> [a] -> [a]
scramble gen offset xs  = let
	(r, gen')       = randomR (0, offset) gen
	(left, x:right) = List.splitAt r xs
	xs'             = scramble gen' offset (left ++ right)
	in x : xs'
