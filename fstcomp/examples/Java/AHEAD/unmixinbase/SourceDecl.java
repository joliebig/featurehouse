

import java.util.*;
import Jakarta.util.*;
import java.io.*;

// the reductions in this class do the following:
// (a) harvests the base, layerName, and fileName
// (b) tags the TypeDeclaration immediately following this declaration
//     for propagateChanges to do something.

public class SourceDecl {
    public void propagateChanges() {
        // Step 1: harvest information from parse tree

        kernelConstants.globals().unmixin.base = ( tok[1].tokenName().equals( "RooT" ) );
        kernelConstants.globals().unmixin.layerName = ( ( AST_QualifiedName ) arg[0] ).GetName();

        String tmp = tok[2].tokenName() ;
        kernelConstants.globals().unmixin.fileName =  Main.uri2file( tmp.substring( 1, tmp.length() - 1 ) ) ;

        // Step 2: now look to the sibling of this AstNode.
        //         label it as extractable, if possible.  if not
        //         possible, report an error
        //         yes, the test is ugly: list linkages are not
        //         in SourceDecl nodes, but in their parent elements.
        //         Here is where we tag the next TypeDeclaration after
        //         a SourceDecl.

        if ( ! ( up.right != null && up.right.arg[0] != null &&
                ( ( TypeDeclaration ) up.right.arg[0] ).canExtract() ) )
            AstNode.fatalError( tok[0],
                       "SoUrCe declaration not matched with code body" );
    }
}
