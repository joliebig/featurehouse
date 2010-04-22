

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

/** production
   [ AST_Modifiers ] UnmodifiedTypeDeclaration   ::ModTypeDecl
*
* @layer<preprocess>
*/

public class ModTypeDecl {

    // returns true only for Ute

    public boolean isExtension() {
        return ( ( UnmodifiedTypeDeclaration ) arg[1] ).isExtension();
    }

    /** returns name of UnmodifiedTypeDecl 
     * @layer<preprocess>
     */
    public String GetName() {
        return ( ( UnmodifiedTypeDeclaration ) arg[1] ).GetName();
    }

    /** returns type signature of UnmodifiedTypeDecl 
     * @layer<preprocess>
     */
    public String GetType() {
        return ( ( UnmodifiedTypeDeclaration ) arg[1] ).GetType();
    }

    /**  composition of ModTypeDecls done in two steps:
         (a) compose modifier lists
         (b) compose unmodified type decls 
    *
    * @layer<preprocess>
    */

    /** composes base ModTypeDecl with extension ModTypeDecl<br>
        composition done in two steps:<br>
         (a) compose modifier lists<br>
         (b) compose unmodified type decls <br>
    */
    public void compose( AstNode etree ) {
        ModTypeDecl e = ( ModTypeDecl ) etree;

        // Step 1: compose modifier lists -- if base list is null
        //         just use the extension list

        arg[0].compose( e.arg[0] );
        
        // Step 2: compose unmodifiedtype declarations

        arg[1].compose( e.arg[1] );
    }
}
