

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

/******************** Main class **************************
 * @layer<preprocess>
 */

   // supply only what is needed by commonbase, which is a layer
   // that sits directly below this layer

public class Main {
    static void usageOftk() {
        System.err.println( "Options: -t  type-sort interface declarations" );
        System.err.println( "         -k  key-sort class field declarations" );
    }
}
