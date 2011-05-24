

import java.util.*;
import java.io.*;
import Jakarta.util.Util2;

//------------------------ j2jSmx layer -------------------
//       encapsulates refinement of state machines and anything
//       to do with their composition.  in this case, the j2j tool
//       requires some rewrites of state machines *prior* to their
//       reduction.  Also, the j2j tool will be able to parse extensions
//       to state machines, but will flag them as errors.

public class UmodSmDecl {

    String previous;

    public void harvestConstructors( int stage ) {

        // Step 0: do nothing if we're inside quoted text

        if ( stage != 0 ) {
            super.harvestConstructors( stage );
            return;
        }

        // Step 1: copy the inheritedCons of $TEqn.kernelConstants.
        //         globals().j2jbase -- these
        //         are the constructors we want to inherit

        copyConstructors();

        // Step 2: now harvest the constructors of the SDClassBody

        arg[3].harvestConstructors( stage );

        // Step 3: add reference to this type declaration

        kernelConstants.globals().j2jbase.previousTypeDecls.add( this );
    }

    public void reduce2java( AstProperties props ) {

        // Step 0: do a normal reduction if we haven't seen SoUrCe decls

        if ( props.getProperty( "SoUrCe" ) == null ) {
            original( props );
            return;
        }

        // Step 1: remember the name of the state machine that is being reduced 
        //         and the name of the class or machine that is being extended.

        String smName = arg[0].tok[0].tokenName();
        props.setProperty( "MixinDeclName", smName );
		  previous = (String) props.getProperty( "ThisName" );
		  if (previous == null) previous = "";
		  props.setProperty( "ThisName", Util2.unmangleId(smName) );
        String extendsName = "";
        SmExtendsClause ec = ( SmExtendsClause ) arg[1].arg[0];
        if ( ec != null )
            extendsName = ec.GetName();
        props.setProperty( "SuperName", extendsName );

        // Step 2: make sure that an FieldDecl is present

        if ( arg[3].arg[4].arg[0] == null ) {

            // can't set it because there is an empty AST_FieldDecl
            // so here's the hack -- we'll add an empty AST_FieldDecl
            // so that we can set its boolean

            arg[3].arg[4].Replace( new  AST_FieldDecl() );
        }
 
        // Step 4: set the addInheritedConstructors boolean

        AST_FieldDecl f = ( AST_FieldDecl ) arg[3].arg[4].arg[0];
        f.addMarker( inheritedCons );
  
        // Step 5: now reduce normally  - and clear the list of
        //         methods that are refined.

        kernelConstants.globals().j2jbase.refinedSet.clear();
        original( props );

        // Step 6: reset the MixinDeclName

        props.setProperty( "MixinDeclName", "" );
        props.setProperty( "SuperName", "" );
		  props.setProperty( "ThisName", previous );
    }
}
