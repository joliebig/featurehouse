import java.util.*;

class cnfClause {

   // returns arraylist of variables whose values are false

   ArrayList getAntecedents( variable v ) {
      ArrayList a = new ArrayList();
      Iterator i = terms.iterator();
      while (i.hasNext()) {
         variable vv = (variable) i.next();
         if (vv == v) continue;
         a.add(vv);
      }
      return a;
   }
}
   
