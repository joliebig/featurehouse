import java.util.*;
import javax.swing.*;

class cnfClause {

    cterm isUnitOpen() {
        cterm openCterm = null;
        Iterator i = terms.iterator();
        while ( i.hasNext() ) {
            cterm t = ( cterm ) i.next();
            switch ( t.eval3() )
            {
                case variable.F:
                // do nothing
                break;
                case variable.T:
                // can't be open
                return null;
                case variable.U:
                if ( openCterm == null )
                    openCterm = t;
                else // more than one term unknown -- can't be open
                    return null;
                break;
			}
        }
        // if we get this far, this is a unit open clause or a unsatisfied clause
        return openCterm;
    }

    boolean isViolated() {
        // all cterms must be false
        Iterator i = terms.iterator();
        while ( i.hasNext() ) {
            cterm t = ( cterm ) i.next();
            if ( t.eval3() != variable.F )
                return false;
        }
        return true;
    }

    // does this clause have a negated term for a variable
    boolean hasNegTerm( boolean neg, variable v ) {
        Iterator i = terms.iterator();
        while ( i.hasNext() ) {
            cterm t = ( cterm ) i.next();
            if ( neg &&  t.negated && t.var == v )
                return true;
            if (!neg && !t.negated && t.var == v )
                return true;
        }
        return false;
    }

    static public Stack stack;

    // BCP algorithm
    static void BCP() {
        while ( !stack.empty() ) {
            cnfClause c = ( cnfClause ) stack.pop();
            cterm t = c.isUnitOpen();
            if ( t != null ) {
                t.var.set(t.negated);
                t.var.justify(c);
            }
        }
    }

   static boolean complete(boolean output) {
      // in 2-valued logic, a complete specification is where
      // all clauses are satisfied

      Iterator i = clist.iterator();
      int cnt = 0;
      String completeMsg = null;

      while (i.hasNext()) {
         cnfClause c = ( cnfClause ) i.next();
         if (!c.eval2()) {
            String msg = c.formula.incompleteMessage;
            if (msg == null)
               msg = c.formulaStr;
            if (completeMsg == null)
               completeMsg = msg;
            else 
               completeMsg += "\n" + msg;
         }
      }
      if (completeMsg == null)
         return true;
		if (output)
         JOptionPane.showMessageDialog( null, completeMsg,
         "Specification Incomplete!", JOptionPane.ERROR_MESSAGE );
      return false;
   }

   // 2-value logic evaluation of a clause -- if any term is
   // true, then the clause is true

   boolean eval2() {
      Iterator i = terms.iterator();
      while ( i.hasNext() ) {
         cterm t = ( cterm ) i.next();
         if (t.eval2())
            return true;
      }
      return false;
   }
}
