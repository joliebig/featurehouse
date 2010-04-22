

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

class PrintableVector extends Vector {
    public PrintableVector() { super();
        ;
    }

    public void print() {
        int i;

        for ( i=0; i<size(); i++ )
            ( ( printTruncObject ) get( i ) ).print();
    }
}
