

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class EStmt {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
