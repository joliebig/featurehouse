

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class program {

    /** compose base tree with extension tree 
     *  @param etree - extension tree
     * @layer<preprocess>
     */
    public void compose( AstNode etree ) {
        // composition involves
        // (a) doing nothing about layer declaration
        // (b) compose the imports lists
        // (c) compose the AST_Class declarations

        // Step 1: do preliminary testing
        //         make sure argument is correct type

        program e = ( program ) etree;

        // Step 2: compose the imports list and AST_Class declarations

        arg[1].compose( e.arg[1] );
        arg[2].compose( e.arg[2] );
    }

    public boolean isExtension() {
        if ( arg[2].arg[0] == null )
            // not TypeDeclaration
            AstNode.fatalError( "file has no root or refinement declaration" );
        return ( ( AST_Class ) arg[2].arg[0] ).isExtension();
    }
}
