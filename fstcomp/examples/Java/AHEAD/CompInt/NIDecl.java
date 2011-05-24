

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

/** production:
InterfaceMemberDeclaration:
  | NestedInterfaceDeclaration::NIDecl 
*
* @layer<CompInt>
*/

public class NIDecl {
    public String signature() {
        return ( ( NestedInterfaceDeclaration ) arg[0] ).signature();
    }

    public boolean actOnHash( Hashtable h ) {
        return ( ( NestedInterfaceDeclaration ) arg[0] ).actOnHash( h );
    }
   
    public void add2Hash( Hashtable h ) {
        ( ( NestedInterfaceDeclaration ) arg[0] ).add2Hash( h );
    }
}
