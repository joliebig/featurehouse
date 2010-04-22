

import java.io.PrintWriter;

//**************************************************
// class AstOptToken extension
//**************************************************
    
public class AstOptToken
    implements AstTokenInterface {

    //**************************************************
    // reduce2ast()
    //**************************************************
    public void reduce2ast( AstProperties props ) {
        PrintWriter ps;

        ps = ( PrintWriter ) props.getProperty( "output" );
        ps.println( " new " +  kernelConstants.LangName + "AstOptToken(" );
        if ( tok[0] != null ) {
            ps.print( ").setParms(" );
            tok[0].reduce2ast( props );
        }
        ps.println( ") /* AstOptToken */" );
    }
}
