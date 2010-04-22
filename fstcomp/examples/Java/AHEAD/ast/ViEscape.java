

import java.io.PrintWriter;

//**************************************************
// ViEscape extension class
//**************************************************
    
public class ViEscape 
    implements  EscapeMarker { // not a list
        
    public void reduce2ast( AstProperties props ) {
        reduce2astEscape( props, "AST_VarInit" );
    }
}
