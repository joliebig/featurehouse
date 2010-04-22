

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class UmodSmExt {
    public void execute( int stage ) {
        if ( stage!=0 ) {
            super.execute( stage );
            return;
        }

        String name = ( ( QName ) arg[0] ).GetName();
        harvest( name,  MMGlobals.StateMachine,  MMGlobals.Interfaces );
        arg[2].execute( stage );
    }
}
