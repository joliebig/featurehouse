

import java.io.PrintWriter;

//**************************************************
// MthEscape extension class
//**************************************************
    
public class MthEscape 
    implements  EscapeMarker,  IsList {
        
    public void reduce2ast( AstProperties props ) {
        reduce2astEscape( props, "AST_FieldDecl" );
    }
}
