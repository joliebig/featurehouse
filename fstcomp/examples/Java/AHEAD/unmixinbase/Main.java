

import java.util.*;
import Jakarta.util.*;
import java.io.*;

/******************** Main class **************************
 * @layer<unmixinbase>
 */

public class Main {
    protected void processing( String label, String fileName ) {
        setBaseURI( new File( fileName ). getAbsoluteFile() . getParent() ) ;
        original( label, fileName );
    }

    protected void outputAST( ArgList argObjects,  AstNode ast ) {
        // this method does the work of UnMixin -- overrides
                  // standard outputAST method and replaces it with this one.
             //
                        
                  // Step 0: convert ast into AST_Program

        AST_Program p = ( AST_Program ) ast;

             // Step 1: unmangle the mangled identifiers
        //
        p.unmangleIds( 0 );
   
        // Step 2: for each SoUrCe/Type declaration pair
        //         propagate chanes back to the original file
        p.propagateChanges();
    }
}
