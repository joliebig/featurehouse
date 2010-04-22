

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public  abstract class AstListNode {
    public void baseRewrite( Hashtable hb, Hashtable he, int stage ) {
        AstNode.fatalError( "AstListNode.baseRewrite" +
                          " method should not be called" );
    }
}
