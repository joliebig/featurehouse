

import java.util.*;
import Jakarta.util.*;
import java.io.*;

public class program {

    public void propagateChanges() {
        AstCursor c = new  AstCursor();
        if ( arg[2].arg[0] == null )
            return;

        // walk through the tree, and invoke propagateChanges on
        // each TypeDeclaration.  Note that SoUrCe declarations tag
        // the TypeDeclaration immediately following it, so that
        // propagateChanges does something other than a no-op.

        for ( c.FirstElement( arg[2].arg[0] ); c.MoreElement(); c.NextElement() )
            ( ( TypeDeclaration ) c.node ).propagateChanges();
    }
}
