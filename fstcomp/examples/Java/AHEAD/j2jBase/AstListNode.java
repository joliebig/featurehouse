

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public abstract class AstListNode {
    public void harvestConstructors( int stage ) {
        AstNode.fatalError( "AstListNode.harvestConstructors( stage ) method should not be called" );
    }
}
