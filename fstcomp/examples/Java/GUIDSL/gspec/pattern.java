import java.util.*;

public class pattern extends gObj {
    static int counter = 0;
    static pattern current;
    public static HashMap Ttable = null;  // contains all patterns
    public ArrayList terms;
    public production myproduction;

    pattern( String name ) {
      super( name );
      current = (pattern) this;
      terms = new ArrayList();
      production.current.pat.add( (pattern) this );
      myproduction=production.current;
      if (counter == 0)
        Ttable = new HashMap();
      Ttable.put( name, (pattern) this );
      counter++;
    }

    public void visit( GVisitor v ) {
        v.action( (pattern) this );
    }

    public void traverse( GVisitor v ) {
        Iterator i = terms.iterator();
        while ( i.hasNext() ) {
            term t = ( term ) i.next();
            t.visit( v );
        }
    }

    public static void dumpTtable() {
        pattern p;
        int cnt = 0;
        System.out.println( "-------Begin Ttable Dump----------" );
        Iterator i = Ttable.values().iterator();
        while ( i.hasNext() ) {
            p = ( pattern ) i.next();
            p.print();
            System.out.println();
            cnt++;
        }
        System.out.println( cnt + " patterns in all." );
        System.out.println( "-------End Ttable Dump----------" );
    }

    public void print() {
       String v = "null";
       if (var!=null) v = var.name; 
       System.out.print(" " +name + " var is " + v );
    }
}
