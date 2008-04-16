

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class SimplePattern {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
