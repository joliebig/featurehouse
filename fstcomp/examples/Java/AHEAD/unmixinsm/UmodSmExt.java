

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class UmodSmExt {
   
    public boolean propagateChanges( ImplementsClause i, 
                                     SmClassBody b ) {

        // do it this way to achieve side effects of oneChange
        boolean u =  UmodSmDecl.oneChange( arg[1], i );
        u =  UmodSmDecl.oneChange( arg[2], b ) || u;
        return u;
    }
}
