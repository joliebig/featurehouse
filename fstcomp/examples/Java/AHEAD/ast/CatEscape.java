

import java.io.PrintWriter;

//**************************************************
// CatEscape extension class
//**************************************************
    
public class CatEscape 
    implements  EscapeMarker { // not a list
        
    public void reduce2ast( AstProperties props ) {
        reduce2astEscape( props, "AST_Catches" );
    }
}
