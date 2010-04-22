

import java.util.Hashtable;
import Jakarta.util.Util2;
import java.io.*;

public  abstract class AstListNode {
    public void mangleLocalIds( int stage ) {
        AstNode.fatalError( "AstListNode.mangleLocalIds(stage) method should not be called" );
    }
}
