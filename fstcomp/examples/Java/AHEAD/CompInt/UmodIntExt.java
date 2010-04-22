

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

   /** production:

UnmodifiedInterfaceExtension
: "interface" QName [ IntExtClause ]
"{" [ InterfaceMemberDeclarations ] "}" ::UmodIntExt
   *
   * @layer<CompInt>
   */

public class UmodIntExt {
    public String GetName() {
        return arg[0].tok[0].tokenName();
    }
    public String GetType() {
        return "interface";
    }

    public void compose( AstNode etree ) {
        // we want to compose two interface extensions.
        // (a) check that both names are the same
        // (b) compose extends lists
        // (c) compose list of member declarations

        UmodIntExt e = ( UmodIntExt ) etree;

        // Step 1: check if both names are the same
         
        String baseName = arg[0].tok[0].tokenName();
        String extName  = e.arg[0].tok[0].tokenName();

        if ( !baseName.equals( extName ) )
            AstNode.fatalError( tok[0],
                        "trying to compose extensions with different names: " +
                    baseName + " and " + extName );
        
        // Step 2: compose extends lists

        arg[1].compose( e.arg[1] );

        // Step 3: now compose interface member declarations

        arg[2].compose( e.arg[2] );
    }
}
