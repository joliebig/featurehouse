

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class ProceedDecl {
    public void reduce2java( AstProperties props ) {

        // Step 1: determine if we are translating a JamPack-produced .jak file

        String tn = tok[0].getTokenName();
        if ( tn.equals( "Proceed" ) ) {
            AstNode.error( tok[0], "Proceed() construct should not be present" );
            return;
        }

        // Step 2: reduce normally

        props.print( tn );
        arg[0].reduce2java( props );
    }
}
