

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

   /** production
   FormalParameter
: [ "final" ] AST_TypeName VariableDeclaratorId::FormParDecl
;
   *
   * @layer<preprocess>
   */

public class FormalParameter {
    public String GetName() {
        AstNode.override( "FormalParameter.GetName", this );
        return "";
    }
}
