

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

/********************* Field Declaration  *****************
 * @layer<preprocess>
 */

   /** production:

   FieldDeclaration<br>
: [ AST_Modifiers ] AST_TypeName AST_VarDecl ";"   ::FldVarDec<br>
;<br>
   */

public class FieldDeclaration {

    /** abstract method -- must be overridden by subclasses.
        purpose is to add signatures to hash table 
        * @layer<preprocess>
        */

    public void add2Hash( Hashtable h ) {
        AstNode.override( "FieldDeclaration.add2Hash", this );
    }

    /** abstract method -- must be overridden by subclasses.
        purpose is to test to see if signatures are already in
        hash table.  If they are, elements will not be added
        to the base program 
        * @layer<preprocess>
        */

    public boolean actOnHash( Hashtable h ) {
        AstNode.override( "FieldDeclaration.actOnHash", this );
        return false;
    }
}
