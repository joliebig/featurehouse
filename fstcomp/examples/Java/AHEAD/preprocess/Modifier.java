

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

/** superclass of all Modifiers 
 * @layer<preprocess>
 */

public class Modifier {

    /** return name of modifier 
     * @layer<preprocess>
     */

    public String GetName() {
        // generic method, can be overridden
        return tok[0].tokenName();
    }
}
