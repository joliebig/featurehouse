

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// this layer is used only for testing if the mmatrix tool
// is processing a mixin-generated file.  Such a file has a SoUrCe
// statement.  If "Root", the token after "SoUrCe" is present, then
// we know that the file defines a constant.  Otherwise it defines a
// refinement

public class SourceDecl {
    static public boolean SourceSeen = false;
    static public boolean isRoot     = false;

    public void execute( int stage ) {
        if ( stage != 0 ) {
            super.execute( stage );
            return;
        }

        // if we've been here before, then don't reset the values

        if ( SourceSeen )
            return;

        // haven't been here before - set the values

        SourceSeen = true;
        if ( tok[1] != null )
            isRoot = true;
    }
}
