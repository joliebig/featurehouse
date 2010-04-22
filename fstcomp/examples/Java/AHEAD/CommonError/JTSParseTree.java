

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class JTSParseTree {

    public void preprocessTree( AST_Program root ) throws Exception {

        // Step 1: check for errors, like ConSuper and SuperPre

        root.checkForErrors( 0, filepath );

        // Step 2: do everything we did before

        original( root );
    }
}
