

import java.util.*;
import Jakarta.util.*;
import java.io.*;

/********************** MIXIN ***********************
 * @layer<mixinbase>
 */

/** Root of base and extension file abstract syntax trees.<br>
  *  
  *  AST_Program<br>
  *     : [ PackageDeclaration ] [ AST_Imports ] [ AST_Class ] 
  *       :: program <br>
**/

public class AST_Program {

    // the following methods must be subclassed -- in essense, 
    // AST_Program is an abstract class.  Question: why isn't AST_Program
    // abstract???  I should try an experiment. --
    // here's the reason: in principle, methods that use override are
    // abstract.  however, there are refinements (i.e., member + method
    // additions) that merely add new members and methods, and don't change
    // the "abstract" nature of a class.  Unfortunately, in a mixin-layer
    // approach, this refinement must be declared abstract becuase the
    // Java compiler will complain if the super classes abstract methods
    // are not defined.  The problem here is that the refinement itself
    // is neutral to being abstract or concrete...  It could be used
    // with a concrete super class, and thus the refinement itself
    // should not be declared abstract... sigh.. so with mixin layers
    // we need to leave this as is.  Perhaps in AHEAD, we can avoid this...

    // performs required formatting suitable for composition of mixins

    public void prepare( JTSParseTree t ) {
        AstNode.override( "AST_Program.prepare", this );
    }

    // performs composition of trees (ala mixin)

    public void compose( AstNode etree,  JTSParseTree base,
            JTSParseTree ext ) {
        AstNode.override( "AST_Program.compose", this );
    }
       
    // this method could be factored out into its own layer...
    // same as in preprocess!

    public boolean isExtension() {
        AstNode.override( "AST_Program.isExtension", this );
        return false;
    }
}
