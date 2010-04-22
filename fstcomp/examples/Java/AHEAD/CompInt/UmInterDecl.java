

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

   /** production
       UnmodifiedInterfaceDeclaration
: "interface" QName [ IntExtClause ]
"{" [ InterfaceMemberDeclarations ] "}"::UmInterDecl
;
   *
   * @layer<CompInt>
   */

public class UmInterDecl {
    public String GetName() {
        return arg[0].tok[0].tokenName();
    }

    public String GetType() {
        return "interface";
    }
      
    public void compose( AstNode etree ) {
        // etree is of type Ute.
        // composition of interface declarations involves four steps.
        // (.) check to see if the names of the base and extension are
        //     the same
        // (a) etree is of type Ute.  get UnmodifiedTypeExtension
        //     as the correct tree argument
        // (b) compose extends lists
        // (c) compose list of member declarations

        // Step 0: check the names
        String bname = arg[0].tok[0].tokenName();
        String ename = etree.arg[0].arg[0].tok[0].tokenName();
        if ( !bname.equals( ename ) )
            AstNode.fatalError( tok[0], 
                       "names of base (" + bname + ") and refinement (" +
                   ename + ") are different:" );

        // Step 1: get correct composition tree

        UmodIntExt e = ( UmodIntExt ) etree.arg[0];
         
        // Step 2: if both the base extends list and extension
        //         extends list are not empty, then compose them
        //         otherwise set the base to be the non-empty list
        //         of the two.

        kernelConstants.globals().isBaseAnInterface = true;
        arg[1].compose( e.arg[1] );

        // Step 3: now compose interface member declarations

        arg[2].compose( e.arg[2] );
    }
}
