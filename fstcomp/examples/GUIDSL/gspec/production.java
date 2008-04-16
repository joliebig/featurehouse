import java.util.*;
import Jakarta.util.*;

public class production extends gObj {
    public static final int unk = -1;
    public static final int opt = 1;
    public static final int plus = 2;
    public static final int star = 3;
    public static final int norm = 0;

    static int counter = 0;
    static production current;
    public int type; // must be { opt, plus, star, norm }
    public ArrayList pat;
    static public HashMap Ptable;

    production( String name ) {
      super( name );

        // Step 1: if this is the first production, then
        //         define a grammar object
        if ( counter==0 ) {
            new grammar( name );
            grammar.rootProduction = (production) this;
            Ptable = new HashMap();
        }
        counter++;

        // Step 2: now define the production itself
        current = (production) this;
        this.type = unk; // unknown type at present
        grammar.productions.put( name,(production)this );
        pat = new ArrayList();
        Ptable.put( name, (production) this );
    }

    String getType() {
        if ( type == opt )
            return "optional";
        if ( type == plus )
            return "plus";
        if ( type == star )
            return "star";
        if ( type == norm )
            return "choose1";
        return "unknown";
    }

    public void visit( GVisitor v ) {
        v.action( (production) this );
    }

    public void traverse( GVisitor v ) {
        pattern p = null;
        Iterator i = pat.iterator();
        while ( i.hasNext() ) {
            Object o = i.next();
            try {
            p = ( pattern ) o;
            } catch( Exception e ) { Util.fatalError( e.getMessage() + " " +
                    o.getClass().getName());
            }
            p.visit( v );
        }
    }

    public static void dumpPtable() {
        production p;
        int cnt = 0;
        System.out.println( "-------Begin Ptable Dump----------" );
        Iterator i = Ptable.values().iterator();
        while ( i.hasNext() ) {
            p = ( production ) i.next();
            p.print();
            System.out.println();
            cnt++;
        }
        System.out.println( cnt + " productions in all." );
        System.out.println( "-------End Ptable Dump----------" );
    }

    public void print() {
       String v = "null";
       if (var!=null) v = var.name; 
       System.out.print(name + " var is " + v + " " + getType());
    }

    static public production find( String name ) {
        return ( production ) production.Ptable.get( name );
    }
}
