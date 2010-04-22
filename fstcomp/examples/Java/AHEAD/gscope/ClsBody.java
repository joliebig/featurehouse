

import java.io.PrintWriter;

//**************************************************
// ClsBody extension class
//**************************************************
    
public class ClsBody {
    public void reduce2java( AstProperties props ) {
        Environment env;

        env = ( Environment ) props.getProperty( "env" );
        super.reduce2java( props );
        if ( env == null )
            props.removeProperty( "env" );
        else
            props.setProperty( "env", env );
    }
}
