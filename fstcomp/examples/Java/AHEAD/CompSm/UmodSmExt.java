

import java.util.*;
import java.io.*;

public class UmodSmExt {

    public void compose( AstNode etree ) {

        // Step 1: do standard checking.  etree is of type UmodSmExt
        //         and remember the names of the base and extension
        //         and that we're starting with an extension.

        if ( ! ( etree instanceof  UmodSmExt ) )
            AstNode.fatalError( tok[0], "Extension of state machine is not of type UmodSmExt" );
         
        UmodSmExt e = ( UmodSmExt ) etree;
        kernelConstants.globals().compclass.isBaseAClass = false;
        String bname =  UmodSmDecl.bname = arg[0].tok[0].tokenName();
        String ename =  UmodSmDecl.ename = e.arg[0].tok[0].tokenName();
        if ( !bname.equals( ename ) )
            AstNode.fatalError( tok[0], "trying to compose state machines with different names: " 
                       + bname + "  " + ename );

        // Step 2: Make sure that there is no root clause in either tree

        if ( arg[2].arg[0].arg[0] != null )
            AstNode.error( tok[0], "state machine extension " + bname + " in " + 
                       getSource() + " should not have " +
                       "Delivery_parameters and Unrecognizable_states clauses" );
        if ( e.arg[2].arg[0].arg[0] != null )
            AstNode.error( tok[0], "state machine extension " + ename +
                       " in " + e.getSource() + " should not have " +
                       "Delivery_parameters and Unrecognizable_states clauses" );
  
        // Step 3: now compose implements clause, and then class body

        arg[1].compose( e.arg[1] );
        arg[2].compose( e.arg[2] );
    }
}
