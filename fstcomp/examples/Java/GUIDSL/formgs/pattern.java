import java.util.*;
import Jakarta.util.*;

class pattern {
   node formula;

   static void makeFormula() {
      Iterator i = Ttable.values().iterator();
      while ( i.hasNext() ) {
         pattern p = ( pattern ) i.next();
         p.formula = p.makef();
      } 
   }

   node makef( ) {
      node n = null;
      Iterator i = terms.iterator();
      while ( i.hasNext() ) {
         term t = ( term ) i.next();
         node tn = null;
         if (t instanceof optprim || t instanceof optprod || t instanceof star)
            tn = new implies( new bterm( t.name ), new bterm( name ));
         else
            tn = new iff( new bterm(name), new bterm( t.name ));
         if (n == null)
            n = tn;
         else
            n = new and( n, tn );
      }
      return n;
   }

   public void print() {
      original();
      System.out.print( " formula = " + formula ); 
   }
}
