

import Jakarta.util.*;
import java.io.*;
import java.util.*;

abstract class AExpr {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
