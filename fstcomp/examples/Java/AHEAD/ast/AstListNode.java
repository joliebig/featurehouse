

import java.io.PrintWriter;

//**************************************************
// class AstListNode extension
//**************************************************
    
public abstract class AstListNode {
    //**************************************************
    // reduce2ast()
    //**************************************************
    public void reduce2ast( AstProperties props ) {
        PrintWriter ps;

        if ( arg[0] != null ) {

            ps = ( PrintWriter ) props.getProperty( "output" );

            // don't output list element instances if
                // they're not needed.  Specifically, don't
                // generate list elements if arg[0] is
                // an escape marker for a list element

            if ( arg[0] instanceof  EscapeMarker &&
                            arg[0] instanceof  IsList ) {
                ps.print( "\n.add( " );
                arg[0].reduce2ast( props );
                ps.println( ") " );
                return;
            }

            ps.print( "\n.add( (" + this.className() + ") new " +
                                     this.className() + "().setParms(" );
            if ( tok != null ) {
                Print_Only_Token_Ast( props );
                ps.println( "," );
            }
            arg[0].reduce2ast( props );
            ps.println( "))/* " + this.className() +" + add */" );
        }
    }
}
