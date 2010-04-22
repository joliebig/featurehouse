

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public abstract class AstListNode {
    public void execute( int stage ) {
        AstNode.fatalError( "AstListNode.execute() method should not be called" );
    }
}
