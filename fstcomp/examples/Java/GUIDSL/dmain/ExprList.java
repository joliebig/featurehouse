

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class ExprList {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
