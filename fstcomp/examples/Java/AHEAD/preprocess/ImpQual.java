

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class ImpQual {

    /* returns name of AST_QualifiedName */

    public String GetName() {
        String result = ( ( AST_QualifiedName ) arg[0] ).GetName();
        if ( arg[1].arg[0] != null )
            result = result + ".*";
        return result;
    }
}
