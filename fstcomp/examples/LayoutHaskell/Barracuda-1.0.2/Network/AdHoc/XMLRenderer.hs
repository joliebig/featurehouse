-- | A module used to render an XML 'Document'.
module Network.AdHoc.XMLRenderer
	(renderDocument
	,escaper
	) where

import Data.List(concatMap)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Escape (XmlEscaper,mkXmlEscaper)

-- | An escaper used to escape special characters, e.g. \'<\', from XML.
escaper :: XmlEscaper
escaper = mkXmlEscaper 
	[('<',"lt")
	,('>',"gt")
	,('&',"amp")
	,('\'',"apos")
	,('\"',"quot")] (\ch -> case ch of
		'<' -> True
		'>' -> True
		'&' -> True
		'\'' -> True
		'\"' -> True
		_ -> False)

-- | Given a 'Document', this function converts it into a fully valid XML
--   'String'.
renderDocument :: Document i -> String
renderDocument (Document prolog tab root msc)
	=  renderProlog prolog
	++ renderSymtab tab
	++ renderElement root
	++ concatMap renderMisc msc

renderProlog :: Prolog -> String
renderProlog (Prolog decl msc1 dtd msc2)
	=  maybe "" renderXMLDecl decl
	++ concatMap renderMisc msc1
	++ maybe "" renderDocTypeDecl dtd
	++ concatMap renderMisc msc2

xmlEscape :: String -> String
xmlEscape ('<':xs) = "&lt;"++xmlEscape xs
xmlEscape ('>':xs) = "&gt;"++xmlEscape xs
xmlEscape ('\"':xs) = "&quot;"++xmlEscape xs
xmlEscape (x:xs) = x:(xmlEscape xs)
xmlEscape "" = ""

renderXMLDecl :: XMLDecl -> String
renderXMLDecl (XMLDecl vers encdec st)
	= "<?xml version=\""++xmlEscape vers++"\""
		++(maybe "" (\(EncodingDecl decl) -> " encoding=\""++xmlEscape decl++"\"") encdec)
		++(maybe "" (\rst -> " standalone=\""++if rst then "yes" else "no"++"\"") st)
		++"?>"

renderMisc :: Misc -> String
renderMisc (Comment str) = "<!--"++xmlEscape str++"-->"
renderMisc (PI (target,str)) = "<?"++target++" "++str++"?>"

renderDocTypeDecl :: DocTypeDecl -> String
renderDocTypeDecl = error "renderDocTypeDecl isn't implemented"
--renderDocTypeDecl (DTD name ext markup) = "<!"++name++maybe "" ((" "++).renderExternalID)++

renderExternalID :: ExternalID -> String
renderExternalID (SYSTEM (SystemLiteral str)) = "SYSTEM \""++xmlEscape str++"\""
renderExternalID (PUBLIC (PubidLiteral str) (SystemLiteral str2)) = "PUBLIC \""++xmlEscape str++"\" \""++xmlEscape str2++"\""

--renderMarkupDecl :: MarkupDecl -> String
--renderMarkupDecl (Element (ElementDecl name ))

renderSymtab :: SymTab EntityDef -> String
renderSymtab = const ""

renderElement :: Element i -> String
renderElement (Elem name attrs cont)
	=  "<"++name
	++ concatMap renderAttribute attrs
	++ (if null cont
		then "/>"
		else ">"++concatMap renderContent cont++"</"++name++">")

renderAttribute :: Attribute -> String
renderAttribute (name,val) = " "++name++"=\""++renderAttValue val++"\""

renderAttValue :: AttValue -> String
renderAttValue (AttValue els) = concatMap (either xmlEscape renderReference) els

renderReference :: Reference -> String
renderReference (RefEntity str) = "&"++str++";"
renderReference (RefChar num) = "&#"++show num++";"

renderContent :: Content i -> String
renderContent (CElem el _) = renderElement el
renderContent (CString _ cd _) = xmlEscape cd
renderContent (CRef ref _) = renderReference ref
renderContent (CMisc msc _) = renderMisc msc
