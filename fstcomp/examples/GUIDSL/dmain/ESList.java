

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class ESList {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
