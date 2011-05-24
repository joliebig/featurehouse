

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

   /** production<br>
   VariableDeclarator<br>
: VariableDeclaratorId [ VarAssign ]::VarDecl<br>
   *
   * @layer<preprocess>
   */

public class VariableDeclarator {

    /** abstract method that returns name of variable 
     * @layer<preprocess>
     */

    public String GetName() {
        AstNode.override( "VariableDeclarator.GetName", this );
        return "";
    }
}
