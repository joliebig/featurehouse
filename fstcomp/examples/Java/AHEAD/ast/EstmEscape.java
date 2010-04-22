

import java.io.PrintWriter;

// gen'd from esctmpl.txt

//**************************************************
// EstmEscape extension class
//**************************************************
    
public class EstmEscape 
    implements  EscapeMarker { // not a list
        
    public void reduce2ast( AstProperties props ) {
        reduce2astEscape( props, "AST_ExpStmt" );
    }
}
