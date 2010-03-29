

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class BImplies {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
