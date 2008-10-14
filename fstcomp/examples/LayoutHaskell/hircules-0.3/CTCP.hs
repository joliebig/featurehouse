module CTCP where

ctcpQuoteChar :: Char
ctcpQuoteChar = '\001'

ctcpQuote :: String -> String
ctcpQuote "" = ""
ctcpQuote cs = ctcpQuoteChar:cs ++ [ctcpQuoteChar]

ctcpUnquote :: String -> String
ctcpUnquote "" = ""
ctcpUnquote ('\001':cs) | last cs == ctcpQuoteChar = init cs
-- ctcpUnquote ('\001':cs) = init cs
ctcpUnquote cs = cs
