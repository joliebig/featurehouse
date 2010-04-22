

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class Modifier {

    /** return name of modifier 
     * @layer<sortfd>
     */

    public String GetName() {
        // generic method, can be overridden
        return tok[0].tokenName();
    }
}
