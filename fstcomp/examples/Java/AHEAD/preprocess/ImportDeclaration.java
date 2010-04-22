

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

   /** ImportDeclaration is an abstract class with abstract method
    *  GetName. Known subclasses is ImpQual <br>

   ImportDeclaration<br>
: "import" AST_QualifiedName [ DotTimes ] ";"    ::ImpQual<br>
;<br>
   *
   * @layer<preprocess>
   */

public class ImportDeclaration {

    /* abstract method that should be overridden by subclasses */

    public String GetName() {
        AstNode.override( "ImportDeclaration.GetName", this );
        return null;
    }
}
