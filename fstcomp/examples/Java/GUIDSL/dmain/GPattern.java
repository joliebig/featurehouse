

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class GPattern {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
