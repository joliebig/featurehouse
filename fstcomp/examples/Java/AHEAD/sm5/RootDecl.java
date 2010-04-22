

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class RootDecl {
    public void reduce2java( AstProperties props ) {

        // Step 1: reduce all sibling constructs as normal

        super.reduce2java( props );

        // Step 2: now make sure that global start and end states
        //         are declared ONLY if a parent is a state declaration
        //         AND there is no ExtendsStateMachine declaration
        //         ancestor 1 = optnode
        //         ancestor 2 = SmClassDecl
        //         ancester 3 = either UmodSmExt or UmodSmDecl

        AstNode n = up.up.up;
        boolean define = false; // default

        if ( n instanceof UmodSmDecl ) {
            AstNode a = n.arg[1].arg[0];
            define = (a!=null && a instanceof SmClsExtends) || a == null;
        }
                                   
        defineStates( define ); // inherited
    }

    // this method could be overridden or extended

    void defineStates( boolean define ) {
        if ( define ) {
            StatesClause.defineState( "start", false, arg[0].tok[0] );
            StatesClause.defineState( "stop", false, arg[0].tok[0] );
        }
    }
}
