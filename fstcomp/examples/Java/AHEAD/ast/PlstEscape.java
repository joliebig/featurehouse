

import java.io.PrintWriter;

//**************************************************
// PlstEscape extension class
//**************************************************
    
public class PlstEscape 
    implements  EscapeMarker,  IsList {
        
    public void reduce2ast( AstProperties props ) {
        reduce2astEscape( props, "AST_ParList" );
    }
}
