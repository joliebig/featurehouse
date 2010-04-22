

import java.util.Hashtable;
import Jakarta.util.Util2;
import java.io.*;

//--------------------- JTSParse Tree -------------------

// this is where localId "hooks" into a main program, by
// extending the preprocessTree method

public class JTSParseTree {

    public void preprocessTree( AST_Program root ) throws Exception {

        // preprocess means -- do everything you did before
        // and then harvest local ids, and then mangle their names

        original( root );
        root.harvestLocalIds();
        root.mangleLocalIds( 0 );
    }
}
