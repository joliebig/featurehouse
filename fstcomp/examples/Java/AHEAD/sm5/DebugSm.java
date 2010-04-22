

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// you to read a x.ser file by typing "<language>.sm$DebugSm x"

public class DebugSm {
    public static void main( String[] args ) {
        sdInfo s;

        s = ( sdInfo )  Utility.readObjectFromFile( args[0] );
        s.print();
    }
}
