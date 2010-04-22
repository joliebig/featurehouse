

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

   /** production
   MethodDeclarator
: QName "(" [ AST_ParList ] ")" [ Dims ]::MthDector
;
   *
   * @layer<preprocess>
   */

public class MethodDeclarator {

    public String GetName() {
        return arg[0].tok[0].tokenName();
    }

    public void setName( String name ) {
        ( ( AstToken ) arg[0].tok[0] ).setName( name );
    }

    public String signature() {
        // Signature of a method is "<name>(<signature of arguments>"

        // Step 1: get name

        String result = arg[0].tok[0].tokenName() + "(";

        // Step 2: add signature of arguments 
        AST_ParList p = ( AST_ParList ) arg[1].arg[0];
        if ( p != null )
            result = result + p.signature();

        return result;
    }
}
