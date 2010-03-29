

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class Optid {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
