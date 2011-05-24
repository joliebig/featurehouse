import java.util.*;
import Jakarta.util.*;

class production {
   node simple;
   node cnf;

   static void makeClauses() {
      Iterator i = Ptable.values().iterator();
      while ( i.hasNext() ) {
         production p = ( production ) i.next();
         p.simple = p.formula.klone().simplify();
         p.cnf    = p.simple.klone().cnf();
         ArrayList al = new ArrayList();
         p.cnf.reduce(al);
         cnfClause.setFormula(al, p.formula); 
         cnfClause.clist.addAll(al);
      }
   }
}
