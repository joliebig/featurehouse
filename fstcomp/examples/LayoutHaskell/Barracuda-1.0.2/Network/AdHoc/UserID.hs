-- |
-- Maintainer: Stephan Friedrichs
module Network.AdHoc.UserID (
	UserID(..)
) where

import Data.Char

-- | Identifies a user in the network.
data UserID = UserID
	{ userName :: String -- ^ The users name.
	, userHost :: String -- ^ The users host.
	}

instance Show UserID where
	show uid = (userName uid) ++ "@" ++ (userHost uid)

instance Eq UserID where
	u1 == u2 = EQ == compare u1 u2

instance Ord UserID where
	compare (UserID n1 h1) (UserID n2 h2) = case compare (map toLower h1) (map toLower h2) of
		EQ -> compare n1 n2
		x  -> x

