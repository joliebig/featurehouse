

import java.io.*;

public class AST_VarDecl {
    static public  AST_VarDecl MakeAST( String in ) {
        try {
	    Parser parser = Parser.getInstance (new StringReader (in)) ;
	    return (AST_VarDecl) parser.parse ("AST_VarDecl") ;
        }
        catch ( ParseException pe ) {
            AstNode.fatalError( "string-to-ast parse error: " + in );
	    return null ;
        }
    }
}
