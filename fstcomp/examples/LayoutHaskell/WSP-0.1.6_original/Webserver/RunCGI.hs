module RunCGI where

import qualified Config as C
import qualified WASH.Utility.BulkIO as B
import WASH.CGI.RawCGIInternal
import WASH.CGI.RawCGI 
import IO
import Request


runCGI h host conf cgiPath pathinfo queryArgs reqString is_head headers cgioptions pgm = do
   let 
     readContents n =
       B.rawGetBytes h n
     locate header_type def = g headers
       where g (ExtensionHeader n v:_) | n == header_type = v
	     g (_:rest) = g rest
	     g [] = def
     content_length = locate "content-length" "0"
     content_type = locate "content-type" "application/x-www-form-urlencoded"
     http_cookie = locate "cookie" ""
     http_accept = locate "accept" ""
     path_info = dropWhile (=='/') pathinfo
     method = read reqString
   contents <- readContents (read content_length)
   let 
     env = CGIEnv	{ serverName = C.serverName conf
       			, serverPort = show (C.port conf)
			, serverSoftware = C.serverSoftware ++ ' ' : C.serverVersion ++ "/WASH"
			, serverProtocol = "HTTP/1.1"
			, scriptName = cgiPath
			, gatewayInterface = "CGI/1.1"
			, requestMethod = method
			, contentLength = content_length
			, contentType = content_type
			, httpCookie = http_cookie
			, httpAccept = http_accept
			, pathInfo = path_info
			, pathTranslated = C.documentRoot conf ++'/':path_info
			, remoteHost = ""
			, remoteAddr = host
			, remoteUser = ""
			, authType = ""
			, rawContents = contents
     			, rawArgs = queryArgs
			, handle = h
			, httpsEnabled = False		    -- hws does not implement it
			}
   startEnv env cgioptions pgm
