

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

   /** production: 
   UnmodifiedTypeDeclaration<br>
: UnmodifiedClassDeclaration<br>
| UnmodifiedInterfaceDeclaration<br>
        ;

   Note: actions for the above productions are defined in CompInt and
   CompClass
   *
   * @layer<preprocess>
   */

public class UnmodifiedTypeDeclaration {

    /** abstract method that returns name of UnmodifiedTypeDeclaration 
     * @layer<preprocess>
     */
    public String GetName() {
        AstNode.override( "UnmodifiedTypeDeclaration.GetName", this );
        return null;
    }

    public boolean isExtension() {
        // class Ute will override -- false is the default response
        return false;
    }

    /** abstract method that returns type signature 
        of UnmodifiedTypeDeclaration 
        * @layer<preprocess>
        */
    public String GetType() {
        AstNode.override( "UnmodifiedTypeDeclaration.GetType", this );
        return null;
    }
}
