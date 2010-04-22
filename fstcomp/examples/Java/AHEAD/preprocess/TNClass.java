

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

/** production:
    : AST_TypeName::TNClass
*
* @layer<preprocess>
*/

public class TNClass {

    /** returns name of AST_TypeName 
     * @layer<preprocess>
     */

    public String GetName() {
        return ( ( AST_TypeName ) arg[0] ).GetName();
    }
}
