

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// the following is used in the first-pass algorithm

public abstract class AstNode {
    public void harvestConstructors( int stage ) {
        int i;
        if ( arg == null )
            return;
        for ( i=0; i<arg.length; i++ )
            if ( arg[i]!=null )
                arg[i].harvestConstructors( stage );
    }
}
