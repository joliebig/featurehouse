

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

//******************************* AST_CLASS *************************/

   /** root of series of class and interface declarations,
    * in general, we assume only one class or one interface defined.<br>
    *
    * AST_Class<br>
    *  : ( TypeDeclaration )+
    *  ;
   *
   * @layer<preprocess>
   */

public class AST_Class {

    /** compose AST_Class base tree with extension AST_Class tree 
     @param etree - extension tree of type AST_Class
     * @layer<preprocess>
     */
    public void compose( AstNode etree ) {

        // Step 1: by the time we get here, this and etree are pointing
             //         to an AST_Class instance, which should be a list of
             //         length one, and that the element of this list is
             //         of type ModTypeDecl.  If not, then we have an
             //         incorrectly formatted .jak file.  So the first
             //         step is to see if the base file is formatted correctly.

        if ( arg[0] == null || arg[0].arg[0] == null || 
                            arg[0].arg[0].right != null || 
                                 ! ( arg[0].arg[0] instanceof  ModTypeDecl ) )
            AstNode.fatalError( "base file has no root or " +
                                                                           "refinement declaration" );

        // Step 2: now do the same for the extension

        AST_Class e = ( AST_Class ) etree;
        if ( e == null || e.arg[0] == null || e.arg[0].arg[0] == null ||
                            e.arg[0].arg[0].right != null ||
                                 ! ( e.arg[0].arg[0] instanceof  ModTypeDecl ) )
            AstNode.fatalError( "extension file has no root or " +
                                                                           "refinement declaration" );
        
        // Step 3: there is both -- compose them.
 
        arg[0].arg[0].compose( e.arg[0].arg[0] );
    }

    // isExtension evaluated over only ModTypeDecl statements
      // by the time isExtension is called, this should be a list
      // of one ModTypeDecl statement.

    public boolean isExtension() {

        if ( arg[0] == null || arg[0].arg[0] == null || 
                            arg[0].arg[0].right != null || 
                                 ! ( arg[0].arg[0] instanceof  ModTypeDecl ) )
            AstNode.fatalError( findToken(), "file has no root or " +
                                                                           "refinement declaration" );
        return ( ( ModTypeDecl ) arg[0].arg[0] ).isExtension();
    }
}
