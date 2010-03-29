

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class BAnd {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
