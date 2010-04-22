

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class QNameType {

    /** returns name of AST_QualifiedName 
     * @layer<preprocess>
     */

    public String GetName() {
        return ( ( AST_QualifiedName ) arg[0] ).GetName();
    }
}
