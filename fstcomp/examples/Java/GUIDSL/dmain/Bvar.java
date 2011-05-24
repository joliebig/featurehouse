

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class Bvar {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
