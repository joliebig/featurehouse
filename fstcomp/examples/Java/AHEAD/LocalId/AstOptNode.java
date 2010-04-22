

import java.util.Hashtable;
import Jakarta.util.Util2;
import java.io.*;

public class AstOptNode {
    public void mangleLocalIds( int stage ) {
        if ( arg[0]!=null )
            arg[0].mangleLocalIds( stage );
    }
}
