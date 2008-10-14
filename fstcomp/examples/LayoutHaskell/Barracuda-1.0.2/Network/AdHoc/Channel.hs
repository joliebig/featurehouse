-- |
-- Maintainer: Stephan Friedrichs, Henning Guenther
module Network.AdHoc.Channel (
	-- * Channel names
	ChannelName,
	mkChannelName,
	anonymous,
	validateChannelName,
	-- * Channel ids
	ChannelID(..)
) where

import Data.Char
import Data.Ord
import qualified Data.Set as Set

-- | The 'ChannelName' type, see 'mkChannelName'.
newtype ChannelName = ChannelName String

-- | Creates a new 'ChannelName'.
mkChannelName :: String -> ChannelName
mkChannelName = ChannelName

-- | Returns the ordering-relevant 'String' of the 'ChannelName'.
getOrd :: ChannelName -> String
getOrd (ChannelName n) = map toLower n

-- | The anonymous 'ChannelName'
anonymous :: ChannelName
anonymous = mkChannelName "Anonymous"

-- | Check 'ChannelName' for validity. Return 'Nothing' if it is alright,
--   'Just' err if it is illegal (err is the reason for that).
validateChannelName :: ChannelName -> Maybe String
validateChannelName cname 
	| len < 1                                   = Just "Sorry, the channel name may not be empty."
	| len > 16                                  = Just "Sorry, the channel name is limited to 16 characters."
	| and (map ((flip Set.member) allowed) str) = Nothing
	| otherwise                                 = Just $ "Sorry, only the following characters are suported in channel names:\n\n"
							++ "\tLetters\tA - Z and a - z\n"
							++ "\tDigits\t0 - 9\n"
							++ "\tUnderscore '_', hyphen '-' and dot '.'"
	where
	allowed = Set.fromList $ ['a'..'z'] ++ ['0'..'9'] ++ ['_', '.', '-']
	str = getOrd cname
	len = length str

instance Show ChannelName where
	show (ChannelName n) = n

instance Eq ChannelName where
	c1 == c2 = EQ == compare c1 c2

instance Ord ChannelName where
	compare = comparing getOrd

-- | The unique 'ChannelID' type identifying each channel.
data ChannelID = ChannelID
	{ channelValue :: String -- ^ A locally set value, unique per host.
	, channelHost  :: String -- ^ The hostname of the 'ChannelID'.
	} deriving (Eq, Ord)

instance Show ChannelID where
	show cid = channelValue cid ++ "@" ++ channelHost cid

