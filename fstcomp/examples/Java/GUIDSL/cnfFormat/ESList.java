class ESList {

    // method walks through the CTable. Each formula
    // is converted into a cnf formula, and then it is
    // converted into CNF format

    public static void toCnfFormat( cnfout out ) throws CNFException {
        int cnt = CTable.size();
        for ( int i = 0; i<cnt; i++ ) {
            node n = ( node ) CTable.get( i );
            out.beginFormula( n );
				node simple = n.klone().simplify();
				out.comment(simple);
				node cnf = simple.klone().cnf();
				out.cnfcomment(cnf);
            cnf.toCnfFormat( out );
            out.endFormula();
        }
    }
}
