

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// ------------------ TypeName List Composition ---------------

   /** production:

   AST_TypeNameList<br>
: TName ( "," TName )*<br>
;<br>

   TName<br>
: AST_TypeName::TNClass<br>
;<br>

   <p>
   TypeNameList is a fairly involved production, much more complicated
   than previous productions.  The idea is to assign a method called
   GetName to TName instances.  We will follow the standard algorithm
   of walking the extension list and seeing if a TName instance belongs
   to the base list.  If so, we remove the TName instance from the base
   list.  When all is done, we concatenate what remains of the extension
   list to the base list.</p>
   <p>
   Now the complication comes when implementing the GetName method
   of TName objects, because GetName calls must be propagated through
   AST_Type name (which could be an AST_QualifiedName or PrimitiveType).
   Yuck.</p>
   <p>
   Note: eventually, the semantics of GetName will be GetQualifiedName
   where we take into account the full path name of an identifier.
   Right now, we're fudging this.</p>
   * @layer<preprocess>
   */

public class AST_TypeNameList {

    // this method is called only on base nodes.
    // returns true if TName x is on the given typename list

    private boolean findTName( TName x ) {
        AstCursor c     = new  AstCursor();
        String         xName = x.GetName();

        for ( c.FirstElement( this ); c.MoreElement(); c.NextElement() ) {
            if ( ( ( TName ) ( c.node ) ).GetName().equals( xName ) )
                return true;
        }
        return false;
    }

    /** compose base AST_TypeNameList with extension AST_TypeNameList 
     * @layer<preprocess>
     */

    public void compose( AstNode etree ) {
        AST_TypeNameList x = ( AST_TypeNameList ) etree;

        // Step 1: foreach element e of extension TypeNameList 
        //         see if e is already present on base TypeNameList list.
        //         if so, delete it from the extension list

        AstCursor c = new  AstCursor();
        for ( c.FirstElement( x ); c.MoreElement(); c.NextElement() ) {
            if ( findTName( ( TName ) c.node ) )
                c.Delete();
        }

        // Step 2: now add the truncated TypeNameList to the base list

        this.add( x, "," );
    }

    /** returns signature of AST_TypeNameList.
        format "type,type,type,"
    * @layer<preprocess>
    */

    public String signature() {
        String result = "";
        AstCursor c = new  AstCursor();
        for ( c.FirstElement( this ); c.MoreElement(); c.NextElement() ) {
            result = result + ( ( TName ) c.node ).GetName() + ",";
        }
        return result;
    }

}
