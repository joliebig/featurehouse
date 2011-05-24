

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class ConDecl {

    public void harvestConstructors( int stage ) {

        // Step 0: do nothing if we are within quoted text

        if ( stage != 0 ) {
            super.harvestConstructors( stage );
            return;
        }

        // Step 1: get the constructor's formal parameter list

        AST_ParList p = ( AST_ParList ) arg[2].arg[0];

        // Step 2: remember this constructor for possible later refinement.
        //         see the reduce2java method of RefCons for details

        if ( p == null )
            kernelConstants.globals().j2jbase.constructorTable.put( "", this );
        else
            kernelConstants.globals().j2jbase.constructorTable.put( p.Signature(), this );

        // Step 3: form the call to super

        String supercall = "{ super(); }";
        if ( p != null )
            supercall= "{ super(" + p.onlyParams() + "); }";
       
        // Step 4: form constructor as a string

        String constructor = arg[0].toString()    // modifiers
                            + arg[1].toString()    // QName
                            + " ( "                // "("
                            + arg[2].toString()    // [AST_ParList]
                            + " ) "                // ")"
                            + arg[3].toString()    // [ThrowsClause]
                            + supercall; // super(...)

        // Step 5: convert into tree; actual constructor is the 
        //         first element on the AST_FieldDecl list

        AST_FieldDecl f =  AST_FieldDecl.MakeAST( constructor );
        ConDecl cd = ( ConDecl ) f.arg[0].arg[0];

        // Step 6: add constructor to those that will be inherited
        //         by all subclasses (again, useful only for Mixin
        //         produced-files

        String sig = "";
        AST_ParList pl = ( AST_ParList ) arg[2].arg[0];
        if ( pl != null )
            sig = pl.Signature();
        kernelConstants.globals().j2jbase.inheritedCons.add( sig, cd, tok[0] );

        // Step 7: finally, search the body of the constructor
        //         for any ConSSuper instances

        arg[4].harvestConstructors( stage );
    }

    // the following marker is needed for correctly translating
	 // "this" constructs.  See ThisPre.jak

    public void reduce2java( AstProperties props ) {
	    props.setProperty( "insideConstructor", "" );
		 super.reduce2java(props);
		 props.removeProperty( "insideConstructor" );
	 }

    public void changeNameTo( String name ) {
        arg[1].tok[0].setTokenName( name );
    }
}
