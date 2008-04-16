

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class Var {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
