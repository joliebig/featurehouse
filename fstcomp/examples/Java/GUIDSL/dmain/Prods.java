

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class Prods {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
