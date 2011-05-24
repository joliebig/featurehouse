

import java.util.*;
import Jakarta.util.*;
import java.io.*;

//---------- code for unmangling ids ----------------

public class QName {
    public void unmangleIds( int stage ) {
        // do nothing if we are inside quoted text

        if ( stage != 0 )
            return;
            
        String id = tok[0].tokenName();
        ( ( AstToken ) tok[0] ).setTokenName( Util2.unmangleId( id ) );
    }
}
