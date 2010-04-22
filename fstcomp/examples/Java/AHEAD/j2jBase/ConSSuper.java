

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

//---------- mixin-layer for ConSSuper ----------------------------

public class ConSSuper  {

    // reduction of Super( type-list).( arg-list ) is simple:
    // replace an instance of the above with:
    //        super( arg-list )

    public void reduce2java( AstProperties props ) {
        props.print( getComment() + "super" );
        arg[1].reduce2java( props );
        props.print( ";" );
    }

    // here's where we propagate constructors UP the refinement chain

    public void harvestConstructors( int stage ) {
        String constructor;
        String sig;

        // Step 0: do nothing if we are within quoted text

        if ( stage != 0 ) {
            super.harvestConstructors( stage );
            return;
        }

        // Step 1: get the constructor to propagate

        if ( arg[0].arg[0] == null ) {
            constructor = "      clsname() { super(); }";
            sig = "";
        }
        else {
            constructor = ( ( AST_TypeNameList ) arg[0].arg[0] )
                           .formConstructor();
            sig = ( ( AST_TypeNameList ) arg[0].arg[0] ).formSignature();
        }
 
        // Step 2: Convert constructor into a tree

        AST_FieldDecl f =  AST_FieldDecl.MakeAST( constructor );
        ConDecl cd = ( ConDecl ) f.arg[0].arg[0];

        // Step 3: propagate constructors up the tree 
        //         here's the trick.  currentTypeDecl.inheritedCons
        //         is the set of all constructors that exists above
        //         the current class (or whatever) that we are translating.
        //         if Super generates a constructor that is in this set
        //         already, its' an error.  Super should refer only
        //         to constructors in the true superclass.  So there's
        //         an unavoidable ambiguity with Mixin-produced files
        //         in this case -- flag it as an error.

        if ( kernelConstants.globals().j2jbase.currentTypeDecl.inheritedCons.containsKey( sig ) ) {
            AstNode.error( tok[0],
                            "Super()() is invoked on a constructor with " +
                        "signature ("+sig+") that already exists in the " +
                        "refinement chain -- use this() constructor call instead" );
            return;
        }
          
        UnmodifiedTypeDeclaration u;
        Vector v =  kernelConstants.globals().j2jbase.previousTypeDecls;
        for ( int i=0; i< v.size(); i++ ) {
            u = ( UnmodifiedTypeDeclaration ) v.elementAt( i );
            u.inheritedCons.union( sig, cd );
        }
    }
}
