

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class RefCons {

    public void reduce2java( AstProperties props ) {

        // Step 0: if we haven't seen a SoUrCe decl, then we have an error.
        //         RefCons should only occur within the context of a mixin-produced file

        if ( props.getProperty( "SoUrCe" ) == null ) {
            AstNode.error( tok[0], 
                        "constructor refinement illegal in non-mixin-produced file" );
            return;
        }

        // Step 1: register the fact that a constructor refinement has been
        //         reduced.  To see the impact, review the reduce2java method
        //         of ConstructorMarker

        String sig = "";
        AST_ParList parlist = ( AST_ParList ) arg[1].arg[0];
        if ( parlist != null )
            sig = parlist.Signature();
        kernelConstants.globals().j2jbase.refinedSet.add( sig );

        // Step 2: generate the class constructor here.  Get a handle on the
        //         original definition (so that we can generate the correct modifiers
        //         and throws clauses), and proceed accordingly.

        ConDecl c = ( ConDecl )  kernelConstants.globals().j2jbase.constructorTable.get( sig );
        if ( c == null ) {
            String name = arg[0].tok[0].tokenName();
            AstNode.error( tok[0], "refining non-existent constructor " + name );
            return;
        }

        String params = "";
        if ( parlist != null )
            params = parlist.onlyParams();

        c.arg[0].print( props ); // modifiers
        String className = ( String ) props.getProperty( "MixinDeclName" );
        props.print( arg[0].getComment() + className + "(" );
        arg[1].print( props ); // arguments
        props.print( ") " );
        c.arg[3].print( props ); // throws
        props.println( "{ super(" + params + "); " );
        arg[2].reduce2java( props ); // refinement code
        props.println( " }" );
    }
}
