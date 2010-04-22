

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

   /** production

   InterfaceMemberDeclaration
: LOOKAHEAD( [ AST_Modifiers() ] "class" )
  NestedClassDeclaration::NCDecl
| LOOKAHEAD( [ AST_Modifiers() ] "interface" )
  NestedInterfaceDeclaration::NIDecl
| LOOKAHEAD( MethodDeclarationLookahead() )
  MethodDeclaration::MDecl
| FieldDeclaration::FDecl
;
   *
   * @layer<CompInt>
   */

public class InterfaceMemberDeclaration {

    public void add2Hash( Hashtable h ) {
        AstNode.override( "InterfaceMemberDeclaration.add2Hash", this );
    }

    public boolean actOnHash( Hashtable h ) {
        AstNode.override( "InterfaceMemberDeclaration.actOnHash", this );
        return false;
    }

    public String signature() {
        // will be overridden by subclasses
        return null;
    }
}
