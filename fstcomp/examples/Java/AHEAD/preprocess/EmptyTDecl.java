

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

/** production:
   | ";"::EmptyTDecl
*
* @layer<preprocess>
*/

public class EmptyTDecl {

    /** returns null 
     * @layer<preprocess>
     */
    public String GetName() {
        return null;
    }

    /** returns null 
     * @layer<preprocess>
     */
    public String GetType() {
        return null;
    }
}
