

import java.io.PrintWriter;

//**************************************************
// XlstEscape extension class
//**************************************************
    
public class XlstEscape 
    implements  EscapeMarker,  IsList {
        
    public void reduce2ast( AstProperties props ) {
        reduce2astEscape( props, "AST_ArgList" );
    }
}
