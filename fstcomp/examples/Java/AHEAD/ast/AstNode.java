

import java.io.PrintWriter;

// Start of code from CodeTemplate.KernelBase
//**************************************************
// class AstNode extension
//**************************************************
    
public abstract class AstNode {

    //**************************************************
    // reduce2ast
    //**************************************************
    public void reduce2ast( AstProperties props ) {
        boolean order[];
        int     t, n, i;
        PrintWriter ps;

        order = printorder();
        t = 0;
        n = 0;
        ps = ( PrintWriter ) props.getProperty( "output" );

        ps.println( " (" + className() + ") new " + className() +
                       "().setParms( " );
        for ( i=0; i<order.length; i++ ) {

            if ( i>0 )
                ps.print( ", " );
            // if order[i] is true; print token else print nonterminal

            if ( order[i] )
                tok[t++].reduce2ast( props );
            else
                arg[n++].reduce2ast( props );
        }
        ps.println( ") /* " + className() + " */" );
    }

    //**************************************************
    // Print_Only_Token_Ast
    //**************************************************
    public void Print_Only_Token_Ast( AstProperties props ) {
        int i;
        PrintWriter ps;

        if ( tok == null )
            return;

        tok[0].reduce2ast( props );
        ps = ( PrintWriter ) props.getProperty( "output" );
        for ( i=1; i<tok.length; i++ ) {
            ps.print( "," );
            tok[i].reduce2ast( props );
        }
    }

    //**************************************************
    // this is the reduce2ast routine that is shared by all
    // code escape rewrites. 
    //**************************************************

    public void reduce2astEscape( AstProperties props, String type ) {
        PrintWriter pw = ( PrintWriter ) props.getProperty( "output" );
        AstToken atok = ( AstToken ) tok[0];
        Integer oldLevel = ( Integer ) props.getProperty( "AstLevel" );

        // Decrement AstLevel marker
        if ( oldLevel != null ) {
            if ( oldLevel.intValue() ==1 )
                props.removeProperty( "AstLevel" );
            else
                props.setProperty( "AstLevel",
                                                                      new Integer( oldLevel.intValue()-1 ) );
        }

        /* DELETE -- always add white space 
                    if (atok.white_space.trim().length() == 0) {
                        // white_space is all white. Don't add comment this.
                        pw.print("(" + $TEqn.kernelConstants.LangName + type + ") " +
                                 $TEqn.kernelConstants.LangName + "AstNode.safeCopy(" );
                        arg[0].reduce2java(props);
                        pw.print(" )");
                    }
                    else {
        */
        pw.print( "(" +  kernelConstants.LangName + type + ") " 
                            +  kernelConstants.LangName  + "AstNode.addComment( " );
        pw.print( kernelConstants.LangName + "AstNode.safeCopy(" );
        arg[0].reduce2java( props );
        pw.print( "),\"" );
        atok.printWhitespaceOnly( props );
        pw.print( "\")" );
        /* DELETE
                    }
        */

                    // Restore AstLevel marker (increment)
        if ( oldLevel == null )
            props.setProperty( "AstLevel", new Integer( 1 ) );
        else
            props.setProperty( "AstLevel", oldLevel );
    }
}
