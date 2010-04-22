

import java.util.Hashtable;
import Jakarta.util.Util2;
import java.io.*;

public class program {
    public void harvestLocalIds() {
        AST_Class c = ( AST_Class ) arg[2].arg[0];
        if ( c != null )
            c.harvestLocalIds();
    }

    public void mangleLocalIds( int stage ) {
        AST_Class c = ( AST_Class ) arg[2].arg[0];
        if ( c != null )
            c.mangleLocalIds( stage );
    }
}
