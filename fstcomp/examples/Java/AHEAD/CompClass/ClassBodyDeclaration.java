

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class ClassBodyDeclaration {

    public boolean isReferenced;
    public boolean isOverridden;

    public void cleanUpBase( AstCursor k, Hashtable he ) {
    // do nothing -- in general
    }

    public void add2Hash( Hashtable h ) {
        isReferenced = false;
        isOverridden = false;
        h.put( signature(), this );
    }

    public String signature() {
        AstNode.override( "ClassBodyDeclaration.signature", this );
        return "";
    }

    public void ok2compose( int stage, Hashtable hb ) {
        AstNode.override( "ClassBodyDeclaration.ok2compose", this );
    }
}
