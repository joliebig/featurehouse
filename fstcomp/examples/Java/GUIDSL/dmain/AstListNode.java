import java.util.*;
import Jakarta.util.*;

class AstListNode {
    public void harvest( Visitor v ) {
        Util.fatalError( "AstListNode.harvest() method should not be called" );
    }
    public void visit( Visitor v ) {
        v.action( this );
    }
}
