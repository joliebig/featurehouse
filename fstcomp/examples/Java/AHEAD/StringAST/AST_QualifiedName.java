

import java.io.*;

public class AST_QualifiedName {
    static public  AST_QualifiedName MakeAST( String in ) {
        try {
	    Parser parser = Parser.getInstance (new StringReader (in)) ;
	    return (AST_QualifiedName) parser.parse ("AST_QualifiedName") ;
        }
        catch ( ParseException pe ) {
            AstNode.fatalError( "string-to-ast parse error: " + in );
	    return null ;
        }
    }
}
