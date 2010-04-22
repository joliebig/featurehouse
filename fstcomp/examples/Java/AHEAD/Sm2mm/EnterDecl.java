

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class EnterDecl {
    public void execute( int stage ) {
        if ( stage!=0 ) {
            super.execute( stage );
            return;
        }

        commonAction( "Enter" );
    }
}
