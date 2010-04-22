

import java.io.PrintWriter;

//**************************************************
// VlstEscape extension class
//**************************************************
    
public class VlstEscape 
    implements  EscapeMarker,  IsList {
        
    public void reduce2ast( AstProperties props ) {
        reduce2astEscape( props, "AST_VarDecl" );
    }
}
