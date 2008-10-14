module ParseToken( identifier, reserved
                 , operator, reservedOp
                        
                 , charLiteral, stringLiteral 
                 , natural, integer, float, naturalOrFloat
                 , decimal, hexadecimal, octal
            
                 , symbol, lexeme, whiteSpace            
             
                 , parens, braces, brackets, squares
                 , semi, comma, colon, dot
                 , semiSep, semiSep1 
                 , commaSep, commaSep1
                 ) where

import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Language as L

myLanguage = 
  L.emptyDef
    { L.commentLine = "#"
    , L.nestedComments = False
    , L.reservedNames = []
    , L.reservedOpNames = []
    , L.caseSensitive = False
    }

myTokenParser =
  T.makeTokenParser myLanguage

identifier	= T.identifier		myTokenParser
reserved	= T.reserved		myTokenParser
operator	= T.operator		myTokenParser
reservedOp	= T.reservedOp		myTokenParser
charLiteral	= T.charLiteral		myTokenParser
stringLiteral	= T.stringLiteral	myTokenParser
natural		= T.natural		myTokenParser
integer		= T.integer		myTokenParser
float		= T.float		myTokenParser
naturalOrFloat	= T.naturalOrFloat	myTokenParser
decimal		= T.decimal	 	myTokenParser
hexadecimal	= T.hexadecimal		myTokenParser
octal		= T.octal		myTokenParser
symbol		= T.symbol		myTokenParser
lexeme		= T.lexeme		myTokenParser
whiteSpace	= T.whiteSpace		myTokenParser
parens		= T.parens		myTokenParser
braces		= T.braces		myTokenParser
brackets	= T.brackets		myTokenParser
squares		= T.squares		myTokenParser
semi		= T.semi		myTokenParser
comma		= T.comma		myTokenParser
colon		= T.colon		myTokenParser
dot		= T.dot			myTokenParser
semiSep		= T.semiSep		myTokenParser
semiSep1 	= T.semiSep1		myTokenParser
commaSep	= T.commaSep		myTokenParser
commaSep1	= T.commaSep1		myTokenParser
