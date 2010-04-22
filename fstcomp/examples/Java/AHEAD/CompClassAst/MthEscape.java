

import java.util.*;

public class MthEscape {
    public void baseRewrite( Hashtable hb, Hashtable he, int stage ) {
        super.baseRewrite( hb, he, stage-1 );
    }

    public void ok2compose( int stage, Hashtable hb ) {
        super.ok2compose( stage-1, hb );
    }
}
