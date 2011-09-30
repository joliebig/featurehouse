// -*- Mode: Java -*-

options {
    CACHE_TOKENS = true ;
    JAVA_UNICODE_ESCAPE = true ;
    //OPTIMIZE_TOKEN_MANAGER = true ;
    STATIC = false ;

    // Section -- options for debugging:
    //
    // DEBUG_PARSER = true ;
    // DEBUG_LOOKAHEAD = true ;
    // DEBUG_TOKEN_MANAGER = true ;
    //
    // FORCE_LA_CHECK = true ;
    // CHOICE_AMBIGUITY_CHECK=5 ;
    // OTHER_AMBIGUITY_CHECK=5 ;
} options

//*****************************************************************************
// Code section to be inserted into the generated parser class:
//*****************************************************************************

code {
    //*************************************************************************
    // Code inserted from "bali.b" source grammar:
    //*************************************************************************

    /**
     * Append the given {@link Token} and any preceding special tokens to a
     * given {@link StringBuffer}.
     *
     * @param token the given JavaCC {@link Token} object
     * @param buffer the buffer to which to append <code>token</code>
     **/
    final private static void accumulate (Token token, StringBuffer buffer) {

	// Append preceding special tokens to <code>buffer</code>:
	//
	Token special = firstSpecial (token) ;
	if (special != token)
	    while (special != null) {
		buffer.append (special.toString ()) ;
		special = special.next ;
	    }

	// Finally, append the token itself:
	//
	buffer.append (token.toString ()) ;
    }
      
    /**
     * Accumulate {@list Token} objects from the token stream, respecting
     * nested code inside <code>open</code> and <code>close</code> pairs,
     * until an unmatched <code>close</code> is the next token in the stream.
     * This method assumes that an <code>open</code> token has just been read
     * from the stream so the initial nesting level is 1.  The method returns
     * when a matching <code>close</code> token is the next token in the token
     * stream.  <em>The <code>close</code> token is left in the stream!</em>
     *
     * @return the accumulated tokens as a {@link String}.
     *
     * @throws ParseException
     * if an end-of-file is found before an unmatched <code>close</code> token.
     **/
    final private Token accumulateNestedRegion (int open, int close)
    throws ParseException {

	StringBuffer buffer = new StringBuffer () ;

	// Initialize result with known information (starting position, etc.):
	//
	Token result = Token.newToken (OTHER) ;
	result.specialToken = null ;

	Token startToken = firstSpecial (getToken (1)) ;
	result.beginColumn = startToken.beginColumn ;
	result.beginLine = startToken.beginLine ;

	// Accumulate tokens until a <code>close</code> token is found:
	//
	for (int nesting = 1 ; nesting > 0 ; ) {

	    token = getToken (1) ;

	    // Update information in result:
	    //
	    result.endColumn = token.endColumn ;
	    result.endLine = token.endLine ;
	    result.next = token.next ;

	    if (token.kind == EOF)
		throw new ParseException (
		    "accumulating from line "
		    + result.beginLine
		    + " at column "
		    + result.beginColumn
		    + ": EOF reached before ending "
		    + tokenImage [close]
		    + " found"
		) ;

	    if (token.kind == open)
		++ nesting ;
	    else if (token.kind == close) {
		if (nesting == 1)
		    break ;
		-- nesting ;
	    }

	    accumulate (token, buffer) ;
	    getNextToken () ;
	}

	result.image = buffer.toString () ;
	return result ;
    }

    /**
     * Accumulate {@link Token} objects from the token stream until a token
     * matching <code>tokenKind</code> is consumed from the stream.  The
     * tokens are accumulated in <code>buffer</code>, including the terminating
     * token.
     *
     * @return a {@link Token}
     * formed by concatenating all intervening tokens and special tokens.
     **/
    final private Token accumulateUntilToken (int tokenKind)
    throws ParseException {

	StringBuffer buffer = new StringBuffer () ;
	Token token = getNextToken () ;

	// Initialize result with known information (starting position, etc.):
	//
	Token result = Token.newToken (OTHER) ;
	result.specialToken = null ;

	Token startToken = firstSpecial (token) ;
	result.beginColumn = startToken.beginColumn ;
	result.beginLine = startToken.beginLine ;

	// Accumulate tokens until a <code>tokenKind</code> token is found:
	//
	while (token.kind != tokenKind) {

	    // Update information in result:
	    //
	    result.endColumn = token.endColumn ;
	    result.endLine = token.endLine ;
	    result.next = token.next ;

	    if (token.kind == EOF)
		throw new ParseException (
		    "from line "
		    + result.beginLine
		    + " at column "
		    + result.beginColumn
		    + ": EOF reached before "
		    + tokenImage [tokenKind]
		    + " found"
		) ;

	    accumulate (token, buffer) ;
	    token = getNextToken () ;
	}

	accumulate (token, buffer) ;

	result.image = buffer.toString () ;
	return result ;
    }

    /**
     * Finds the first token, special or otherwise, in the list of special
     * tokens preceding this {@link Token}.  If this list is non-empty, the
     * result will be a special token.  Otherwise, it will be the starting
     * token.
     *
     * @param token the given {@link Token}.
     * @return the first special token preceding <code>token</code>.
     **/
    final private static Token firstSpecial (Token token) {

	while (token.specialToken != null)
	    token = token.specialToken ;

	return token ;
    }
} code

//*****************************************************************************
// Lexical tokens start here:
//*****************************************************************************

"{"			LBRACE
"}"			RBRACE
"<"			OPENANGLE
">"			CLOSEANGLE
"("			OPENPAREN
")"			CLOSEPAREN
"code"			_CODE
"options"		_OPTIONS
"EOF"			_EOF
"IGNORE_CASE"		_IGNORE_CASE
"JAVACODE"		_JAVACODE
"LOOKAHEAD"		_LOOKAHEAD
"MORE"			_MORE
"PARSER_BEGIN"		_PARSER_BEGIN
"PARSER_END"		_PARSER_END
"SKIP"			_SKIP
"SPECIAL_TOKEN"		_SPECIAL_TOKEN
"TOKEN"			_TOKEN
"TOKEN_MGR_DECLS"	_TOKEN_MGR_DECLS

TOKEN :
{
	<BALI_TOKEN: <UPPERCASE> (<UPPERCASE> | <DIGIT>)*>
|
	<#UPPERCASE: ["A"-"Z", "_", "$"]>
|
	<STRING:
		"\""
		( (~["\"","\\","\n","\r"])
		| ("\\"
		    ( ["n","t","b","r","f","\\","'","\""]
		    | ["0"-"7"] ( ["0"-"7"] )?
		    | ["0"-"3"] ["0"-"7"] ["0"-"7"]
		    )
		  )
		)*
		"\""
	>
|
	<INTEGER: (<DIGIT>)+>
}


//*****************************************************************************
// Java code productions start here:
//*****************************************************************************

JAVACODE
CodeBlockNode codeBlockNode (Token token) {
    return (new CodeBlockNode ()) . setParms (t2at (token)) ;
}

JAVACODE
CodeBlockNode findBlockBegin () {
    return codeBlockNode (accumulateUntilToken (LBRACE)) ;
}

JAVACODE
CodeBlockNode findBlockEnd () {
    return codeBlockNode (accumulateNestedRegion (LBRACE, RBRACE)) ;
}

JAVACODE
CodeBlockNode findCloseAngle () {
    return codeBlockNode (accumulateNestedRegion (OPENANGLE, CLOSEANGLE)) ;
}

JAVACODE
CodeBlockNode findCloseParen () {
    return codeBlockNode (accumulateNestedRegion (OPENPAREN, CLOSEPAREN)) ;
}


//*****************************************************************************
// Bali productions start here:
//*****************************************************************************

// Symbols defined in JAVACODE sections:
//
require findBlockBegin -> CodeBlockNode ;
require findBlockEnd   -> CodeBlockNode ;
require findCloseAngle -> CodeBlockNode ;
require findCloseParen -> CodeBlockNode ;

BaliParse
	: [Options] [ParserCode] [Statements]		:: BaliParseNode
	;

Options
	: _OPTIONS Block _OPTIONS			:: OptionsNode
	;

ParserCode
	: _CODE Block _CODE				:: ParserCodeNode
	;

Block
	: "{" findBlockEnd "}"				:: BlockNode
	;

Statements
	: (Statement)+
	;

Statement
	: BaliGrammarRule
	| BaliTokenDefinition
	| JavacodeProduction
	| RegexTokenDefinition
	| TokenManagerDeclarations
	;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// The simple statement types:
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

BaliTokenDefinition
	: STRING BALI_TOKEN				:: BaliTokenDefineNode
	;

JavacodeProduction
	: _JAVACODE ScanBlock				:: JavacodeNode
	;

TokenManagerDeclarations
	: _TOKEN_MGR_DECLS ":" ScanBlock		:: TokenManagerNode
	;

ScanBlock
	: findBlockBegin findBlockEnd "}"		:: ScanBlockNode
	;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Statement type -- Bali grammar rule:
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

BaliGrammarRule
	: IDENTIFIER ":" Productions ";"		:: BaliGrammarNode
	;

Productions
	: Production ("|" Production)*
	;

Production
	: [Lookahead] Rewrite				:: ProductionNode
	;

Lookahead
	: _LOOKAHEAD "(" findCloseParen ")"		:: LookaheadNode
	;

Rewrite
	: "(" [Lookahead] Primitive ")" "+"		:: SimpleListNode
	| Primitive PrimitiveRewrite			:: PrimitiveRewriteNode
	;

PrimitiveRewrite
	: "(" [Lookahead] Primitive Primitive ")" "*"	:: ComplexListNode
	| [Pattern] [ClassName]				:: PatternNode
	;

Pattern
	: (Primitive)+
	;

ClassName
	: "::" IDENTIFIER				:: ClassNameNode
	;

Primitive
	: "[" [Lookahead] Terminal "]"			:: OptionalNode
	| Terminal
	;

Terminal
	: BALI_TOKEN					:: BaliTokenNode
	| IDENTIFIER					:: IdentifierNode
	| STRING					:: StringNode
	;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Statement type -- JavaCC token definition:
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RegexTokenDefinition
	: [StateSet] REKind [CaseFlag] ":" "{" REList "}"
							:: RegexDefinitionNode
	;

StateSet
	: "<" StatesSpecifier ">"			:: StatesNode
	;

StatesSpecifier
	: "*"						:: StarStatesNode
	| StatesList                                    :: ListStatesNode
	;

StatesList
	: StateName ("," StateName)*
	;

StateName
	: BALI_TOKEN					:: StateNameNode
	;

REKind
	: _TOKEN					:: TokenKindNode
	| _SPECIAL_TOKEN				:: SpecialKindNode
	| _SKIP						:: SkipKindNode
	| _MORE						:: MoreKindNode
	;

CaseFlag
	: "[" _IGNORE_CASE "]"				:: CaseFlagNode
	;

REList
	: RegexBlock ("|" RegexBlock)*
	;

RegexBlock
	: Regex [Block] [NextState]			:: RegexBlockNode
	;

NextState
	: ":" BALI_TOKEN				:: NextStateNode
	;

Regex
	: STRING					:: StringRegexNode
	| "<" AngleRegex				:: AngleRegexNode
	;

AngleRegex
	: LOOKAHEAD(2) BALI_TOKEN ">"			:: BaliRegexNode
	| LOOKAHEAD(2) [Label] ComplexRegex		:: ComplexRegexNode
	;

ComplexRegex
	: LOOKAHEAD(2) STRING ">"			:: StringComplexNode
	| findCloseAngle ">"				:: AngleComplexNode
	;

Label
	: ["#"] BALI_TOKEN ":"				:: LabelNode
	;
