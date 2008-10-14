-- -----------------------------------------------------------------------------
-- Copyright 2002, Simon Marlow.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
-- 
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
-- 
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
--  * Neither the name of the copyright holder(s) nor the names of
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- -----------------------------------------------------------------------------

module ConfigParser where

import Char
import ParseToken
import Text.ParserCombinators.Parsec
import Config

type ConfigBuilder = Config -> Config

parseConfig :: String -> IO (Either ParseError ConfigBuilder)
parseConfig fname
  = do input <- readFile fname
       return $ parse configParser fname (dropComments input)

dropComments "" = ""
dropComments ('#':xs) = dropComments (dropWhile (/='\n') xs)
dropComments ('\"':xs) = '\"': copyToEOL xs
dropComments (x:xs) = x: dropComments xs

copyToEOL "" = ""
copyToEOL ('\n':xs) = '\n': dropComments xs
copyToEOL (x:xs) = x: copyToEOL xs

ws_char = oneOf (" \t")
ws = many ws_char
ws1 = many1 ws_char

configParser :: Parser ConfigBuilder
configParser = do
  whiteSpace
  cs <- many (do {c <- configLine; newLine; whiteSpace; return c})
  eof
  return (foldr (.) id cs)

newLine = ws >> char '\n'

configLine :: Parser ConfigBuilder
configLine
 = do (reserved "userdir"                   >> p_userDir)
  <|> (reserved "user"                >> p_user)
  <|> (reserved "group"                  >> p_group)
  <|> (reserved "serveradmin"            >> p_serverAdmin)
  <|> (reserved "servername"             >> p_serverName)
  <|> (reserved "serveralias"            >> p_serverAlias)
  <|> (reserved "serversignature"            >> p_ignoreString)
  <|> (reserved "usecanonicalname"       >> p_useCanonicalName)
  <|> (reserved "documentroot"           >> p_documentRoot)
  <|> (reserved "servertype"             >> p_ignoreString)
  <|> (reserved "serverroot"             >> p_ignoreString)
  <|> (reserved "lockfile"               >> p_ignoreString)
  <|> (reserved "pidfile"               >> p_ignoreString)
  <|> (reserved "scoreboardfile"               >> p_ignoreString)
  <|> (reserved "keepalivetimeout"       >> p_keepAliveTimeout)
  <|> (reserved "keepalive"               >> p_ignoreString)
  <|> (reserved "maxkeepaliverequests"               >> p_ignoreString)
  <|> (reserved "maxspareservers"               >> p_ignoreString)
  <|> (reserved "minspareservers"               >> p_ignoreString)
  <|> (reserved "startservers"               >> p_ignoreString)
  <|> (reserved "maxrequestsperchild"               >> p_ignoreString)
  <|> (reserved "include"               >> p_ignoreString)
  <|> (reserved "clearmodulelist"       >> return id)
  <|> (reserved "addmodule"               >> p_ignoreString)
  <|> (reserved "loadmodule"               >> p_ignoreString2)
  <|> (reserved "extendedstatus"               >> p_ignoreString)
--  <|> (reserved "davlockdb"               >> p_ignoreString)
--  <|> (reserved "xbithack"               >> p_ignoreString)
  <|> (reserved "customlog"			>> p_ignoreString2)
  
-- 
  <|> (reserved "port"                   >> p_port)
  <|> (reserved "maxclients"             >> p_maxClients)
  <|> (reserved "timeout"                >> p_timeout)
-- 
  <|> (reserved "directoryindex"         >> p_directoryIndex)
  <|> (reserved "accessfilename"         >> p_accessFileName)
  <|> (reserved "typesconfig"            >> p_typesConfig)
  <|> (reserved "scriptalias"		 >> p_scriptAlias)

  <|> (reserved "defaulttype"            >> p_defaultType)
  <|> (reserved "hostnamelookups"        >> p_hostnameLookups)
  <|> (reserved "errorlog"               >> p_errorLog)
  <|> (reserved "loglevel"               >> p_logLevel)
  <|> (reserved "logFormat"		 >> p_logFormat)
  <|> (reserved "accesslogfile"          >> p_accessLogFile)
  <|> (reserved "accesslogformat"        >> p_accessLogFormat)
  <|> (reserved "listen"                 >> p_listen)
  <|> (reserved "addlanguage"            >> p_addlanguage)
  <|> (reserved "languagepriority"       >> p_languagepriority)
  <|> (p_element >> return id)

p_user  = do str <- p_lit_or_string; return (\c -> c{user = str})
-- 
p_timeout = do i <- int; return (\c -> c{requestTimeout = i})
p_keepAliveTimeout = do i <- int; return (\c -> c{keepAliveTimeout = i})
p_maxClients  = do i <- int; return (\c -> c{maxClients = i})
p_port = do i <- int; return (\c -> c{port = i})
p_group = do str <- p_lit_or_string; return (\c -> c{group = str})

p_serverAdmin = do str <- p_lit_or_string; return (\c -> c{serverAdmin = str})
p_serverName = do str <- p_lit_or_string; return (\c -> c{serverName = str})
p_serverAlias = do str <- p_lit_or_string
		   return (\c -> c{serverAlias = str : serverAlias c})
p_useCanonicalName = do b <- bool; return (\c -> c{useCanonicalName = b})
p_documentRoot = do str <- p_lit_or_string; return (\c -> c{documentRoot = str})

p_ignoreString = do str <- p_lit_or_string; return (\c -> c)
p_ignoreString2 = do str <- p_lit_or_string; str2 <- p_lit_or_string; return (\c -> c)

p_logFormat = do p_lit_or_string; p_lit_or_string; return id
-- 
p_scriptAlias = do str <- p_lit_or_string; 
		   str1 <- p_lit_or_string; 
		   return (\c -> c{scriptAlias = Just (str, str1)})
p_userDir = do str <- p_lit_or_string; return (\c -> c{userDir = str})
p_directoryIndex = do str <- p_lit_or_string; return (\c -> c{directoryIndex = str})
p_accessFileName = do str <- p_lit_or_string; return (\c -> c{accessFileName = str})
p_typesConfig = do str <- p_lit_or_string; return (\c -> c{typesConfig = str})
p_defaultType = do str <- p_lit_or_string; return (\c -> c{defaultType = str})
p_hostnameLookups = do b <- bool; return (\c -> c{hostnameLookups = b})
p_errorLog = do str <- p_lit_or_string; return (\c -> c{errorLogFile = str})
p_logLevel = do i <- (ll <|> int); return (\c -> c{logLevel = i})
p_accessLogFile = do str <- p_lit_or_string; return (\c -> c{accessLogFile = str})
p_accessLogFormat = do str <- p_lit_or_string; return (\c -> c{accessLogFormat = str})
p_listen = do i <- int; return (\c -> c{listen = i : listen c})
p_addlanguage = do lang <- p_lit_or_string; ext <- p_lit_or_string; return (\c -> c{addLanguage = (lang,ext) : addLanguage c})
p_languagepriority = do langs <- many p_lit_or_string; return (\c -> c{languagePriority = langs})

bool = do { string "On"; return True } 
   <|> do { string "Off"; return False }

ll :: Parser Int
ll =       do { string "debug"; return 7 }
       <|> do { string "info";  return 6 }
       <|> do { string "notice"; return 5 }
       <|> do { string "warn"; return 4 }
       <|> do { string "error"; return 3 }
       <|> do { string "crit"; return 2 }
       <|> do { string "alert"; return 1 }
       <|> do { string "emerg"; return 0 }

int :: Parser Int
int = do str <- many1 digit; return (read str)

p_element = do symbol "<"
	       tag <- many alphaNum
	       p_rest tag
p_rest tag =do many (satisfy (/='>'))
	       symbol ">"
	       let p_contents =
		         (symbol "<" >>
		            ((symbol "/" >>
			       ((symbol tag >> symbol ">")))
			    <|> do{ newt <- many alphaNum; p_rest newt; p_contents }))
		     <|> (p_lit_or_string >> p_contents)
		     <|> (ws >> p_contents)
		     <|> (many (noneOf "<\"#") >> p_contents)
	       p_contents

p_literal = do { many1 (satisfy (\c -> not (isSpace c || c `elem` "\"<>"))) }

p_string = do { char '\"'; str <- many (noneOf "\n\"\\"); char '\"'; return str }

p_lit_or_string = ws >> ( p_literal <|> p_string )
