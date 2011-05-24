

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class Opts {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
