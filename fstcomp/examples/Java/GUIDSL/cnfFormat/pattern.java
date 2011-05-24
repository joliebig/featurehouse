import java.util.*;

class pattern {

    // method walks through the pattern Table, for each pattern
    // convert it into cnf formula, and then output the
    // converted formula into CNF format

    public static void toCnfFormat( cnfout out ) throws CNFException {
        Iterator i = Ttable.values().iterator();
        while ( i.hasNext() ) {
            pattern p = ( pattern ) i.next();
            node simple = p.formula.klone().simplify();
                node cnf = simple.klone().cnf();
            out.beginFormula( p.formula );
                out.comment(simple);
                out.cnfcomment(cnf);
            cnf.toCnfFormat( out );
            out.endFormula();
        }
    }
}
