

import java.io.PrintWriter;

//**************************************************
// class AstOptNode extension
//**************************************************
    
public class AstOptNode {
    //**************************************************
    // reduce2ast()
    //**************************************************
    public void reduce2ast( AstProperties props ) {
        PrintWriter ps;

        ps = ( PrintWriter ) props.getProperty( "output" );
        ps.println( " new "+  kernelConstants.LangName + "AstOptNode(" );
        if ( arg[0] != null ) {
            ps.print( ").setParms(" );
            arg[0].reduce2ast( props );
        }
        ps.println( ") /* AstOptNode */" );
    }
}
