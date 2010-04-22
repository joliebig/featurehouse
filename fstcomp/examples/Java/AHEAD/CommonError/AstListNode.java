

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public abstract class AstListNode {
    public void checkForErrors( int stage, String file ) {
        AstNode.fatalError( "in file " + file + 
                     " AstListNode.checkForErrors() method should not be called" );
    }
}
