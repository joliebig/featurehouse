

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

/************************* for sorting ************************
 * @layer<preprocess>
 */

public class AstCursor 
          implements Cloneable {

    public  AstCursor copy() {
        AstCursor c = null;
        try {
            c = ( AstCursor ) this.clone();
        }
        catch ( Exception e ) {
            AstNode.fatalError( "clone of AstCursor failed" );
        }

        return c;
    }
}
