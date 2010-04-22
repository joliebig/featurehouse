

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// this layer is present if preprocess, gscope, and sortfd
// are present in the same equation.  Otherwise, it is absent

public class Env {

    public void setSortKey() {
        sortkey = "005";
    }
 
    public String signature() {
        return "Env-";
    }

    public void add2Hash( Hashtable h ) {
        h.put( "Env-", this );
    }

    public void ok2compose( int stage, Hashtable hb ) {

        // Step 0: Do nothing if we are inside quoted text

        if ( stage != 0 )
            return;

        // Step 1: get corresponding environment in base

        Env benv = ( Env ) hb.get( "Env-" );

        // Step 2: if there was no environment decl, do nothing.
        //         if there is, then merge the lists and delete
        //         the env declaration in the base.

        if ( benv != null ) {
            AstList lb = ( AstList ) benv.arg[0].arg[0];
            AstList le = ( AstList ) arg[0].arg[0];

            if ( le != null && lb != null ) {
                lb.setComment( " , " );
                le.add( lb );
            }
            else
                if ( le == null )
                    arg[0].arg[0].Replace( lb );
            benv.Delete();
        }
    }
}
