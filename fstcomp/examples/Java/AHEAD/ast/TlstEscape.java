

import java.io.PrintWriter;

//**************************************************
// TlstEscape extension class
//**************************************************
    
public class TlstEscape 
    implements  EscapeMarker,  IsList {
        
    public void reduce2ast( AstProperties props ) {
        reduce2astEscape( props, "AST_TypeNameList" );
    }
}
