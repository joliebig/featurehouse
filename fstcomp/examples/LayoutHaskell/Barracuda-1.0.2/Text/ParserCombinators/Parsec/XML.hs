-- |
-- Maintainer: Henning Guenther
--
-- Helper functions to use a Parsec-Parser for XML documents.
module Text.ParserCombinators.Parsec.XML
	(XMLParser
	,anyContent
	,content
	,element
	,text
	,namedElement
	,namedElementWithAttrs
	,stringElement
	,recurse
	,recurseElements
	) where

import Text.XML.HaXml.Posn
import qualified Text.XML.HaXml.Pretty as Pretty
import Text.XML.HaXml.Types

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Error hiding (Message)

-- | A Parser for XML 'Content'.
type XMLParser a = GenParser (Content Posn) () a

getPosn :: Content Posn -> SourcePos
getPosn cont = let
	posn = case cont of
		CElem _ p	-> p
		CString _ _ p	-> p
		CRef _ p	-> p
		CMisc _ p	-> p
	in newPos (posnFilename posn) (posnLine posn) (posnColumn posn)

classifyContent :: Content a -> String
classifyContent cont = (case cont of
	CElem _ _ -> "element"
	CString _ _ _ -> "string"
	CRef _ _ -> "reference"
	CMisc _ _ -> "misc") ++ "(" ++ (show $ Pretty.content cont) ++ ")"

-- | Accepts any content.
anyContent :: XMLParser (Content Posn)
anyContent = content (Just)

-- | Accepts content verified by a function.
content :: (Content Posn -> Maybe a) -> XMLParser a
content f = token classifyContent getPosn f

-- | Forces an 'Element' as next token.
element :: XMLParser (Element Posn)
element = content (\cont -> case cont of
	CElem el _ -> Just el
	_ -> Nothing)
	<?> "element"

-- | Forces plain text as next token.
text :: XMLParser String
text = content (\cont -> case cont of
	CString _ str _ -> Just str
	_ -> Nothing)
	<?> "text-node"

-- | Parses an element with given name. Returns the xml 'Attribute's.
namedElementWithAttrs :: String -> XMLParser ([Attribute],[Content Posn])
namedElementWithAttrs name = content (\cont -> case cont of
	CElem (Elem ename attr conts) _
		| ename == name	-> Just (attr,conts)
		| otherwise	-> Nothing
	_ -> Nothing)
	<?> "element \""++name++"\""

-- | As 'namedElementWithAttrs', ony that the attributes are ignored.
namedElement :: String -> XMLParser [Content Posn]
namedElement name = namedElementWithAttrs name >>= return.snd

-- | Parses an element with a given name. The text content is returned.
stringElement :: String -> XMLParser String
stringElement name = namedElement name >>= recurse (option "" text)

-- | Helper function to recurse through an XML document.
recurseElements :: XMLParser a -> [Content Posn] -> XMLParser a
recurseElements p conts = recurse p [ el | el@(CElem _ _) <- conts ]

-- | Helper function to recurse through an XML document.
recurse :: XMLParser a -> [Content Posn] -> XMLParser a
recurse p conts = do
	inp <- getInput
	setInput conts
	res <- p
	setInput inp
	return res

