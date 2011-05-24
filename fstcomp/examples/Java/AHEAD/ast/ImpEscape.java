

import java.io.PrintWriter;

//**************************************************
// ImpEscape extension class
//**************************************************
    
public class ImpEscape 
    implements  EscapeMarker,  IsList {
        
    public void reduce2ast( AstProperties props ) {
        reduce2astEscape( props, "AST_Imports" );
    }
}
