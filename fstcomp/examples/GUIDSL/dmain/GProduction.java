

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class GProduction {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
