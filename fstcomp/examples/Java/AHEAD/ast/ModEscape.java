

import java.io.PrintWriter;

//**************************************************
// ModEscape extension class
//**************************************************
    
public class ModEscape 
    implements  EscapeMarker,  IsList {
        
    public void reduce2ast( AstProperties props ) {
        reduce2astEscape( props, "AST_Modifiers" );
    }
}
