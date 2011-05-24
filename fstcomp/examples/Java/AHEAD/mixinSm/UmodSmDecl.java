

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

/************ mixinSm layer ****************
 * @layer<mixinSm>
 */

public class UmodSmDecl {

    // this method links extension declaration as a "extends" to
    // the base declaration (whose typedeclaration name) is "name"

    public void extensionOf( String name ) {
        // add "name" to the extension list of this interface

        // Step 1: create an AST_QualifiedName

        AST_QualifiedName tl =  AST_QualifiedName.Make( " " + name + " " );

        // Step 2: now create an extension and plug it in
  
        if ( arg[1].arg[0] == null ) {
            AstOptNode aon = ( AstOptNode ) arg[1];
            aon.setParms( new  ExtClause().setParms( new  AstToken().setParms( " ","extends", 0 ),  tl ) );
        }
        else {
            AstNode.fatalError( tok[0], "state_machine " + getName() + 
                        " already has an extends clause" );
        }
    }

    // this mangles the state machine name, mangles the name of constructors
    // and returns the mangled name

    public String getAndMangleName() {
        // get the name of the interface, mangle it, and return the
        // mangled name

        String name = mangleName( getName() );
        setName( name );

        AST_FieldDecl f = ( ( AST_FieldDecl ) arg[3].arg[4].arg[0] );
        if ( f != null )
            f.mangleConstructors();

        return name;
    }

    public String getName() {
        return arg[0].tok[0].tokenName();
    }

    private void setName( String name ) {
        ( ( AstToken ) arg[0].tok[0] ).setName( name );
    }
}
