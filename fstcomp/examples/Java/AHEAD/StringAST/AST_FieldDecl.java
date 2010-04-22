

import java.io.*;

public class AST_FieldDecl {
    static public  AST_FieldDecl MakeAST( String in ) {
        try {
	    Parser parser = Parser.getInstance (new StringReader (in)) ;
	    return (AST_FieldDecl) parser.parse ("AST_FieldDecl") ;
        }
        catch ( ParseException pe ) {
            AstNode.fatalError( "string-to-ast parse error: " + in );
	    return null ;
        }
    }
}
