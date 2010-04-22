

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class TNClass {
    public String GetSignature() {
        AST_TypeName tn = ( AST_TypeName ) arg[0];
        return tn.GetSignature();
    }
}
