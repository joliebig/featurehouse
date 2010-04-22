

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class sm4data {
    static final String serExtension = ".ser";

    sdInfo Sm; // parse info repository
    String       ser_directory = ""; // output directory
    Hashtable    serCache = new Hashtable(); // cache of .ser files
    int          uniqueCounter = 0; // unique integer

    // convert AST into a string, where null ASTs are "" strings
    // these are not really standard functions -- if they were,
    // they would be removed to the StringAST layer.

    static String ToString( AstNode x ) {
        return ( x == null ) ? "" : x.toString();
    }

    // methods to convert strings into different ASTs, where
    // vacuous strings are mapped to null ASTs

    static  AST_FieldDecl ToFieldDecl( String x ) {
        return ( x.equals( "" ) || x.equals( "\n" ) ) ? null : 
                 AST_FieldDecl.MakeAST( x );
    }

    static  AST_Stmt ToStmt( String x ) {
        return ( x.equals( "" ) || x.equals( "\n" ) ) ? null : 
                 AST_Stmt.MakeAST( x );
    }

    // refineMethod( o, u, m, v, h, t ) is used for refining Prepare, Exit,
    // Enter, Otherwise, Otherwise_default, Test, and Action methods.
    // upon return of refineMethod, set o = u.
    // t is a token whose line number is extracted for error reporting

    static  AstNode refineMethod( AstNode orig,  AstNode upd, 
                                String specName, String methodName, 
                                boolean isVoid,  AstTokenInterface t ) {
        boolean proceedFound;
        String extraMeth;

		  // Step 0: make sure that upd is not null.  if so, provide
		  //         dummy statement

		  if (upd == null)
		     upd = AST_Stmt.MakeAST( ";" );

        // Step 1: see if this is the first specification in this file
        //         note: when states and edges are first declared, 
        //         bodies are present for each method spec (e.g., Enter, Exit,
        //         etc.  So orig == null only if we are dealing with 
        //         a spec method refinement in a subclass (or subSm).

        if ( orig == null ) {
            // Step 2: it is the first specification in this file.  Search upd
            //         for Proceeds declaration.  Replace "Proceed" with
            //         " super.<methodName>". <specName> example is
            //         "Prepare", <methodName> example is "Prepare_myState".

            proceedFound = replaceProceed( upd, " super." + methodName );
            if ( !proceedFound && isVoid ) {

                // make upd an implicit before method, which is by default
                // what it should be

                upd = ToStmt( upd + " super." + methodName + "( " +
                              kernelConstants.globals().sm4vars.Sm.argdecl + "); " );
            }
            else
                if ( !proceedFound && !isVoid )
                    AstNode.error( t, Utility.SourceName() + "Proceed() not found in " +
                           specName + " method." );
        }
        else {
            // Step 3: it isn't the first specification in this file.  
            //         Create a mangled name to house the original 
            //         specification code
          
            String mname = methodName + "$$" + 
                                kernelConstants.globals().sm4vars.Sm.name +
                           +  kernelConstants.globals().sm4vars.uniqueCounter++;

            // Step 4: now replace each "Proceed" with "<mname>" and
            //         add the generated method to extra_methods for output

            proceedFound = replaceProceed( upd,  " " + mname );
            if ( isVoid )
                extraMeth = "\n   final void ";
            else
                extraMeth = "\n   final boolean ";
            extraMeth = extraMeth + mname + "( " + 
                             kernelConstants.globals().sm4vars.Sm.pardecl + 
                        " ) {";
            if ( isVoid )
                extraMeth = extraMeth + orig + " }";
            else
                extraMeth = extraMeth + " return " + orig + "; }";

            // we're done if upd was updated. If not, assume upd is
            // an before method

            if ( !proceedFound && isVoid )
                upd = ToStmt( upd + mname + "( " + 
                              kernelConstants.globals().sm4vars.Sm.argdecl + "); " );
            else
                if ( !proceedFound && !isVoid )
                    AstNode.error( t, Utility.SourceName() + "Proceed() not found in " +
                      specName + " method." );
            kernelConstants.globals().sm4vars.Sm.extra_methods = 
                      kernelConstants.globals().sm4vars.Sm.extra_methods + extraMeth;
        }
        return upd;
    }

    private static boolean replaceProceed( AstNode tree, String code ) {
        AstCursor c    = new  AstCursor();
        boolean proceedFound = false;

        if ( tree == null )
            return false;

        for ( c.First( tree ); c.More(); c.PlusPlus() ) {
            if ( c.node instanceof  ProceedDecl ) {
                ProceedDecl n = ( ProceedDecl ) c.node;

                // this kinda cheats -- should replace it with the actual
                // parse tree, but this should be OK.

                n.tok[0].setTokenName( code );
                proceedFound = true;
            }
        }
        return proceedFound;
    }
}
