import java.util.*;
import Jakarta.util.*;

class pattern {
   node simple;
   node cnf;

   static void makeClauses() {
      Iterator i = Ttable.values().iterator();
      while ( i.hasNext() ) {
         pattern p = ( pattern ) i.next();
         p.simple = p.formula.klone().simplify();
         p.cnf    = p.simple.klone().cnf();
         ArrayList al = new ArrayList();
         p.cnf.reduce(al);
         cnfClause.setFormula(al, p.formula);
         cnfClause.clist.addAll(al);
      }
   }
}
