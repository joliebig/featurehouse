

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class JTSParseTree {

    // preprocessTree and phase2 are exactly the same as common

    public void compose( JTSParseTree t ) {
        // apply inheritance composition rule: const o whatever = const
        // we could do better here, and make suer that the name of the
        // const class, interface, etc. matches the name of whatever.
        // in fact, we should.  this will do for now.

        if ( t.isExtension() )
            root.compose( t.root );
        else {
            AstNode.warning( "overrides previously defined file" );
            root = t.root;
        }
    }
}
