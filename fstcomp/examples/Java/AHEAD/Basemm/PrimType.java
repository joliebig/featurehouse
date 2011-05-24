

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class PrimType {
    public String GetSignature() {
        String sig = arg[0].tok[0].tokenName();
        if ( arg[1].arg[0] != null )
            sig = sig + ( ( Dims ) arg[1].arg[0] ).GetSignature();
        return sig;
    }
}
