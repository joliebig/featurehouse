

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class TermList {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
