

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class BOr {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
