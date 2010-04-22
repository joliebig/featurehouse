

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

/** production:
InterfaceMemberDeclaration:
  | NestedClassDeclaration::NCDecl 
*
* @layer<CompInt>
*/

public class NCDecl {
    public String signature() {
        AstNode.fatalError( tok[0], 
                 "CompInt cannot compose nested classes" );
        return "";
    }
    public boolean actOnHash( Hashtable h ) {
        AstNode.fatalError( tok[0], 
                  "CompInt cannot compose nested classes" );
        return false;
    }
    public void add2Hash( Hashtable h ) {
        AstNode.fatalError( tok[0], 
                 "CompInt cannot compose nested classes" );
    }
}
