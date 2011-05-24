

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

   // note: we are using GetName here but really, we want to use
   //       the full-path name of arguments eventually.

   /** production
   AST_ParList
: FormalParameter ( "," FormalParameter )*
;
   *
   * @layer<preprocess>
   */
 
public class AST_ParList {

    public String signature() {
        AstCursor c = new  AstCursor();
        String         result = "";

        for ( c.FirstElement( this ); c.MoreElement(); c.NextElement() ) {
            result = result + ( ( FormalParameter ) c.node ).GetName() + ",";
        }

        return result;
    }
}
