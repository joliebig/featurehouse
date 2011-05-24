

import java.io.PrintWriter;

//**************************************************
// IdEscape extension class
//**************************************************
    
public class IdEscape 
    implements  EscapeMarker,  IsList {
        
    public void reduce2ast( AstProperties props ) {
        reduce2astEscape( props, "AST_QualifiedName" );
    }
}
