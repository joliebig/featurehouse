import java.util.*;

public class grammar extends gObj {
    public static grammar current;
    public static production rootProduction;
    public static HashMap productions;

    grammar( String name ) {
      super( name );
        productions = new HashMap();
        rootProduction = null;
        production.counter = 0;
        current = (grammar) this;
    }

    public void visit( GVisitor v ) {
        v.action( this );
    }

    public void traverse( GVisitor v ) {
        rootProduction.visit( v );
    }
}
