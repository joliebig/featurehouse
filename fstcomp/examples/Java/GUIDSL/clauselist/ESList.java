import java.util.*;

class ESList {
   static void makeClauses() {
	   Iterator i = CTable.iterator();
		while (i.hasNext()) {
         node n = ( node ) i.next();;
         node simple = n.klone().simplify();
         node cnf = simple.klone().cnf();

         ArrayList al = new ArrayList();
         cnf.reduce(al);
         cnfClause.setFormula(al, n);
         cnfClause.clist.addAll(al);
      }
   }
}


