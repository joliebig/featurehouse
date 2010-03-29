

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class Pats {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
