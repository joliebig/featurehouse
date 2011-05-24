

import java.util.Hashtable;
import Jakarta.util.Util2;
import java.io.*;

public  abstract class AstList {
    public void mangleLocalIds( int stage ) {
        AstNode l;
        if ( arg[0]==null )
            return;
        for ( l = arg[0]; l!=null; l = l.right ) {
            if ( l.arg[0] == null )
                continue;
            l.arg[0].mangleLocalIds( stage );
        }
    }
}
