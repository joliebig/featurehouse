module Channel (Channel, IRCChannel(..))
where

import Gtk

type Channel = String

data IRCChannel = IRCChan { chanbuffer :: TextBuffer
                          , channame :: String
                          , chanreal :: Bool
                          , chanbox :: Container
                          , chanend :: TextMark
                          , channick :: TextMark
                          , chanentry :: TextMark
                          , chanview :: TextView
                          , chanusers :: [String]
                          , chantopic :: String
                          , chancoding :: Maybe String}
