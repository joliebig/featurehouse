

import java.io.*;

public class AST_Modifiers {
    static public  AST_Modifiers MakeAST( String in ) {
        try {
	    Parser parser = Parser.getInstance (new StringReader (in)) ;
	    return (AST_Modifiers) parser.parse ("AST_Modifiers") ;
        }
        catch ( ParseException pe ) {
            AstNode.fatalError( "string-to-ast parse error: " + in );
	    return null ;
        }
    }
}
