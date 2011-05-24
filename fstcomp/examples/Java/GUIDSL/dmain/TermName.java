

import Jakarta.util.*;
import java.io.*;
import java.util.*;

class TermName {

    public void visit( Visitor v ) {
        
        v.action( this );
    }

}
