

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// Note: composing optional nodes is a bit complicated.
//       in effect, it is the union if either one is empty,
//       else, it is the result of their composition.
//       if it is their composition, we have to recurse, returning
//       the updated optional node.

public class AstOptNode {

    public void setSource( String s ) {
        if ( _source == null )
            _source = s;
        if ( arg[0]!=null )
            arg[0].setSource( s );
    }

    public void compose( AstNode etree ) {

        // Step 1: have eArg point to child of optional node

        AstNode eArg = null;
        if ( etree != null )
            eArg = etree.arg[0];

        // Step 2: bArg is arg[0]

        if ( arg[0] != null && eArg != null )
            arg[0].compose( eArg );
        else
            if ( eArg != null )
                arg[0] = eArg;
    }
}
