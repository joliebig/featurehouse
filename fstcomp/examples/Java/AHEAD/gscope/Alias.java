

import java.io.PrintWriter;

//**************************************************
// Alias extension class
//**************************************************
    
public class Alias {
    public void reduce2java( AstProperties props ) {
        PrintWriter ps;

        ps = ( PrintWriter ) props.getProperty( "output" );
        ps.print( "\n\t((" +  kernelConstants.LangName + "Environment) _E).addAlias(\"" );
        AstToken.printWhitespace( false );
        arg[0].reduce2java( props );
        AstToken.printWhitespace( true );
        ps.print( "\", " );
        arg[1].reduce2java( props );
        ps.print( ");\n" );
    }
}
