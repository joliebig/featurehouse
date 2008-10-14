-- |
-- Maintainer: Stephan Friedrichs
--
-- A generalization of collections storing timestamped data.

module Barracuda.TimedCollection (
	TimedCollection(..)
) where

import Data.Time

-- | Represents a collection that contains timestamped information. This
--   collection can be purged by deleting all information older than a given
--   point of time.
class TimedCollection t where
	-- | Deletes all information before the given timestamp.
	deleteBefore :: UTCTime -> t -> t

