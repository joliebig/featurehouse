

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class ConstructorDeclaration {

    public String signature() {
        AstNode.override( "ConstructorDeclaration.signature", this );
        return "";
    }

    public void add2Hash( Hashtable h ) {
        AstNode.override( "ConstructorDeclaration.add2Hash", this );
    }
         
    public void compose( AstNode etree ) {
        AstNode.override( "ConstructorDeclaration.compose", this );
    }
}
