

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class QNameType {
    public String GetSignature() {
        AST_QualifiedName n = ( AST_QualifiedName ) arg[0];
        String sig = n.GetName();
        if ( arg[1].arg[0] != null )
            sig = sig + ( ( Dims ) arg[1].arg[0] ).GetSignature();
        return sig;
    }
}
