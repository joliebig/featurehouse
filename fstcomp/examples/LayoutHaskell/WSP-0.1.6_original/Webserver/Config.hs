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

module Config where

-----------------------------------------------------------------------------
-- Config info

data Config = Config {
  user			:: String,
  group			:: String,
  
  port			:: Int,
  listen		:: [Int], -- Todo: [(String,Int)],  
				  -- eg. listen www.haskell.org:80
  
  requestTimeout	:: Int,
  keepAliveTimeout	:: Int,
  maxClients		:: Int,
  
  serverAdmin		:: String,	-- "" indicates no admin
  serverName		:: String,	-- "" indicates no canon name
  serverAlias		:: [String],
  useCanonicalName	:: Bool,
  hostnameLookups	:: Bool,
  
  documentRoot		:: String,
  userDir		:: String,
  directoryIndex	:: String,
  accessFileName	:: String,
  indexes		:: Bool,
  followSymLinks	:: Bool,
  
  scriptAlias		:: Maybe (String, String),
  
  typesConfig		:: String,
  defaultType		:: String,
  
  addLanguage		:: [(String,String)],
  languagePriority	:: [String],

  accessLogFile		:: String,
  accessLogFormat	:: String,

  errorLogFile		:: String,
  logLevel		:: Int
  }
#ifdef DEBUG
  deriving Show
#endif

defaultConfig :: Config
defaultConfig = Config{
  user = "nobody",
  group = "nobody",
  
  port = 80,
  listen = [],
  
  requestTimeout	= 300,
  keepAliveTimeout	= 15,
  maxClients		= 150,
  
  serverAdmin		= "",
  serverName		= "",
  serverAlias		= [],
  useCanonicalName	= False,
  hostnameLookups	= False,
  
  documentRoot		= "/usr/local/www/data",
  userDir		= "",
  directoryIndex	= "index.html",
  accessFileName	= ".htaccess",
  indexes		= False,
  followSymLinks	= False,
  
  scriptAlias		= Nothing,
  
  typesConfig		= "/etc/mime.types",
  defaultType		= "text/plain",
  
  addLanguage		= [],
  languagePriority	= ["en"],

  accessLogFile		= "http-access.log",
  accessLogFormat	= "%h %l %u %t \"%r\" %s %b \"%{Referer}i\" \"%{User-Agent}i\"",

  errorLogFile		= "httpd-error.log",
  logLevel		= 1
  }

-- not user-definable...
serverSoftware       = "HWS+WASH"
serverVersion	     = "0.2"
