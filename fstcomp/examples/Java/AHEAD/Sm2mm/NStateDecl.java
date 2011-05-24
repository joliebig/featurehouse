

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class NStateDecl {
    public void execute( int stage ) {
        if ( stage!=0 ) {
            super.execute( stage );
            return;
        }

        String name = ( ( QName ) arg[0] ).GetName();
        add( name, true, this );
    }
}
