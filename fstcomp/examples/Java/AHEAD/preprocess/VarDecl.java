

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class VarDecl {

    /** returns name of variable 
     * @layer<preprocess>
     */

    public String GetName() {
        return arg[0].arg[0].tok[0].tokenName();
    }
}
