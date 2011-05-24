import java.util.*;

class production {

    // method walks through the PTable, for each production
    // convert it into cnf formula, and then output the
    // converted formula into CNF format

    public static void toCnfFormat( cnfout out ) throws CNFException {
        Iterator i = Ptable.values().iterator();
        while ( i.hasNext() ) {
            production p = ( production ) i.next();
            node simple = p.formula.klone().simplify();
            node cnf = simple.klone().cnf();
            out.beginFormula( p.formula ); //original
            out.comment( simple );
            out.cnfcomment( cnf );
            cnf.toCnfFormat( out );
            out.endFormula();
        }
    }
}
