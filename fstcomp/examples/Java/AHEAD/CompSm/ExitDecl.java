

import java.util.*;
import java.io.*;

public class ExitDecl {

    public void harvestAst( LinkedList ll ) {
        // harvest the block for ExitDecl
        ll.add( arg[1] );
    }

    public void add2Hash( Hashtable h, String source ) {
        verifyState( h, "Exit" );
    }
}
