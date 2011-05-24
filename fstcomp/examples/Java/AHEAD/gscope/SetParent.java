

import java.io.PrintWriter;

//**************************************************
// SetParent extension class
//**************************************************
    
public class SetParent {
    public void reduce2java( AstProperties props ) {
        PrintWriter ps;

        ps = ( PrintWriter ) props.getProperty( "output" );
        ps.print( "_E.addEnv(" );
        arg[0].reduce2java( props );
        ps.print( ".getEnv());\n" );
    }
}
