

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public abstract class AstList {
    public void checkForErrors( int stage, String file ) {
        AstNode l;
        if ( arg[0]==null )
            return;
        for ( l = arg[0]; l!=null; l = l.right ) {
            if ( l.arg[0] == null )
                continue;
            l.arg[0].checkForErrors( stage, file );
        }
    }
}
