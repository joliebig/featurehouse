

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public abstract class AstListNode {

    public void setSource( String s ) {
        AstNode.fatalError( "AstListNode.setSource( s ) method should not be called" );
    }

    public void compose( AstNode etree ) {
        AstNode.fatalError( "AstListNode.cmpose(etree) method should " +
                         "not be called" );
    }
}
