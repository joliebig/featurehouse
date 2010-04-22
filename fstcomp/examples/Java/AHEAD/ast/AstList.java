

import java.io.PrintWriter;

//**************************************************
// class AstList extension
//**************************************************
    
public abstract class AstList {
    //**************************************************
    // reduce2ast()
    //**************************************************
    public void reduce2ast( AstProperties props ) {
        AstNode l;
        PrintWriter ps;

        // Step 1: generate the AstList node
        ps = ( PrintWriter ) props.getProperty( "output" );

        ps.println( " (" + className() + ") new " + className() + "()" );

        // Step 3: return if the list is empty

        if ( arg[0] == null )
            return;

        // Step 3: print all the same

        for ( l = arg[0]; l != null; l = l.right )
            l.reduce2ast( props );
    }
}
