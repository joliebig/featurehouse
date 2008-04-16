import Jakarta.util.*;
import java.util.*;

public class cnfClause {
   public static ArrayList clist = new ArrayList();  // List of all cnfClauses

   public ArrayList terms;
   node      formula;
   String    formulaStr;

   public cnfClause( ) {
      terms = new ArrayList();
      formula = null;
      formulaStr = null;
   }

   static void setFormula( ArrayList terms, node cnf ){
      String s = cnf.toString();

      Iterator i = terms.iterator();
      while (i.hasNext()) {
         cnfClause t = ( cnfClause ) i.next();
         t.formula = cnf;
         t.formulaStr = s;
      }
   }

   void add( cterm c ) { terms.add(c); }

   void setFormula( node cnf ) {
      formula = cnf;
      formulaStr = cnf.toString();
   }
      

   // for debugging
   void print() {
      System.out.println("formula = " + formulaStr);
		Iterator i = terms.iterator();
      while (i.hasNext()) {
         cterm t = ( cterm ) i.next();
         t.print();
      }
      System.out.println();
   }

   static void dumpCList() {
      System.out.println("Dumping clist");
		Iterator i = clist.iterator();
		int cnt = 0;
		while (i.hasNext()) {
         cnfClause c = ( cnfClause ) i.next();
         System.out.print(cnt++ + " ");
         c.print();
      }
      System.out.println();
   }
} 
