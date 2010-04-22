

import java.util.*;
import Jakarta.util.*;
import java.io.*;

public class AstOptNode {
    public void unmangleIds( int stage ) {
        if ( arg[0]!=null )
            arg[0].unmangleIds( stage );
    }
}
