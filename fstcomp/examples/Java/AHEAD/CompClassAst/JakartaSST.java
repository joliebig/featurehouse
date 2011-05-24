

import java.util.*;

// the following classes are examples of rewrites that
// note boundaries between quoted text and executable text.
// These rewrites are necessary only if both the CompClass layer
// and the ast layer are present; they are absent otherwise.

public class JakartaSST {
    public void baseRewrite( Hashtable hb, Hashtable he, int stage ) {
        super.baseRewrite( hb, he, stage+1 );
    }

    public void ok2compose( int stage, Hashtable hb ) {
        super.ok2compose( stage+1, hb );
    }
}
