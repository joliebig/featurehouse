

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class ConsStmt {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
