

import java.util.*;
import Jakarta.util.*;
import java.io.*;

public class ConDecl {
    public void mangleConstructor() {
        String className = argTokenName( 1 );
        String mangledClassName = Util2.mangleId( className, _source );
        setArgTokenName( 1,mangledClassName );
    }
}
