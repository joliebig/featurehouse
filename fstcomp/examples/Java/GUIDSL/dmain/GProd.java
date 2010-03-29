

import Jakarta.util.*;
import java.io.*;
import java.util.*;

abstract class GProd {

    public void visit( Visitor v ) {

        v.action( this );
    }

}
