// -*- Mode: Java -*-
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Adds the "require" statement to the grammar.  The form of a "require"
// statement is:
//
//	require Rule1, Rule2 ;
//	require Rule3 -> Type ;
//
// where "Rule1", "Rule2" and "Rule3" are rule names and "Type" is a type name.
// A type name must ultimately resolve to a Java class name (e.g., an AST
// node).  The meaning of a "require" statement is that each specified rule is
// defined in another grammar file.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

"require"		_REQUIRE

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Statement type -- required grammar rules ("external"):
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

require Statement ;

Statement
	: RequireStatement
	;

RequireStatement
	: _REQUIRE RequireRules ";"			:: RequireStatementNode
	;

RequireRules
	: RequireRule ("," RequireRule)*
	;

RequireRule
	: IDENTIFIER [RequireType]			:: RequireRuleNode
	;

RequireType
	: "->" IDENTIFIER				:: RequireTypeNode
	;
