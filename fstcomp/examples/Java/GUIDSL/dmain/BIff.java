

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class BIff {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
