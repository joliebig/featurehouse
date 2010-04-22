

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

/************* Kernel extensions *********************
 * @layer<Basemm>
 */

public abstract class AstNode {
    public void execute( int stage ) {
        int i;
        if ( arg == null )
            return;
        for ( i=0; i<arg.length; i++ )
            if ( arg[i]!=null )
                arg[i].execute( stage );
    }

    // adds to vector v the string names of every AST_Qualified
    // name found in the parse tree

    public void harvestAST_QualifiedNames( HashSet v ) {
        AstCursor c = new  AstCursor();
        for ( c.First( this ); c.More(); c.PlusPlus() ) {
            if ( c.node instanceof  AST_QualifiedName ) {
                String name = ( ( AST_QualifiedName ) c.node ).GetName();

                // this wierdness is added because of mixin-produced files.
                // we don't harvest generated names.  A generated name will
                // contain two dollar signs.

                if ( name.indexOf( "$$" ) == -1 )
                    v.add( ( ( AST_QualifiedName ) c.node ).GetName() );
                c.Sibling();
            }
        }
    }
}
