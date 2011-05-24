

import java.util.*;
import Jakarta.util.*;
import java.io.*;

public abstract class AstListNode {
    public void unmangleIds( int stage ) {
        AstNode.fatalError( "AstListNode.unmangleIds(stage) method should not be called" );
    }
}
