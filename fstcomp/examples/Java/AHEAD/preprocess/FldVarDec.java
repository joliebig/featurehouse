

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class FldVarDec {

    /** add signatures to hash table h 
     * @layer<preprocess>
     */

    public void add2Hash( Hashtable h ) {
        ( ( AST_VarDecl ) arg[2] ).add2Hash( h );
    }

    /** only called on nodes of extension tree; 
        extension fields are compared with those defined in 
        base -- if field exists in both places, we have an error. 
        * @layer<preprocess>
        */

    public boolean actOnHash( Hashtable h ) {
        // method only called on nodes of extension tree
        return ( ( AST_VarDecl ) arg[2] ).actOnHash( h );
    }

    /** this must be fixed - this is where actonHash comes in..*
     * @layer<preprocess>
     */
    public String signature() {
        return "";
    }
 
}
