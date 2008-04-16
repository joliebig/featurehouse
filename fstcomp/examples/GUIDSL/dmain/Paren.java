

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class Paren {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
