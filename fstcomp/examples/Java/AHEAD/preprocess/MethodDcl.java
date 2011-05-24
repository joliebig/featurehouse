

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class MethodDcl {

    public String signature() {
        return ( ( MethodDeclarator ) arg[2] ).signature();
    }

    public String GetName() {
        return ( ( MethodDeclarator ) arg[2] ).GetName();
    }

    public void setName( String name ) {
        ( ( MethodDeclarator ) arg[2] ).setName( name );
    }

    public void addModifier( Modifier m ) {
        // Step 1: if modifier list is empty, create a list

        if ( arg[0].arg[0] == null ) {
            AST_Modifiers l = new  AST_Modifiers();
            l.add( new  AST_ModifiersElem().setParms( m ) );
            arg[0].arg[0] = l;
            return;
        }

        // Step 2: else add to modifier list

        ( ( AST_Modifiers ) arg[0].arg[0] ).addModifier( m );

    }

    public void compose( AstNode etree ) {
        // composition of method declarations involves three items
        // (a) composition of AST_Modifiers
        // (b) composition of Throws Clauses
        // (c) composition body

        // Step 1: compose modifier lists -- if base list is null
        //         just use the extension list

        MethodDcl e = ( MethodDcl ) etree;
        arg[0].compose( e.arg[0] );

        // Step 2: compose Throws Clauses -- do the same as above
        //         extra level of indirection needed to go through
        //         the ThrowsClause object to access the AST_TypeNameList

        arg[3].compose( e.arg[3] );

        // Step 3: compose bodies

        arg[4].compose( e.arg[4] );
    }
}
