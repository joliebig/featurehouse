

import java.io.PrintWriter;

//**************************************************
// TypEscape extension class -- never appears on a list
//**************************************************
    
public class TypEscape {
        
    public void reduce2ast( AstProperties props ) {
        reduce2astEscape( props, "AST_TypeName" );
    }
}
