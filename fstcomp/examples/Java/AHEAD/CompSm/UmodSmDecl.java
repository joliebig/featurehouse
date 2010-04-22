

import java.util.*;
import java.io.*;

public class UmodSmDecl {
    static public String bname;
    static public String ename;

    public void compose( AstNode etree ) {

        // Step 1: do standard checking.  etree is of type Ute

        if ( ! ( etree instanceof  Ute ) )
            AstNode.fatalError( tok[0], "Extension of state machine is not of type UmodSmExt" );
         
        UmodSmExt e = ( UmodSmExt ) etree.arg[0];
        kernelConstants.globals().compclass.isBaseAClass = true;
        bname = arg[0].tok[0].tokenName();
        ename = e.arg[0].tok[0].tokenName();
        if ( !bname.equals( ename ) )
            AstNode.fatalError( tok[0], "trying to compose state machines with different names: " 
                       + bname + "  " + ename );

        // Step 2: if the state machine extends another state machine
        //         then there cannot be a root clause otherwise
        //         there must be a root clause.

        SmExtendsClause ec = ( SmExtendsClause ) arg[1].arg[0];
        if ( ec != null && ec instanceof  SmExtends ) {
            // there is an extends clause (i.e., the state machine
            // is extending another state machine).  This means that
				// a root clause is optional.  (in future versions,
				// root clauses will be mandatory).
        }
        else {
            // the state machine is the root of an extension hierarchy.
            // it must have a root clause
            if ( arg[3].arg[0].arg[0] == null )
                AstNode.error( tok[0],  "state machine " + bname + 
                           " must have a root clause" );
        }

        // Step 3: now compose implements clause, and then class body

        arg[2].compose( e.arg[1] );
        arg[3].compose( e.arg[2] );
    }
}
