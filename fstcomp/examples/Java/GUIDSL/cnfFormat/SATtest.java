class SATtest {

    // returns # of variables in test as well as outputs the
    // CNF file contents
    //
    public void toCnfFormat( cnfout out ) throws dparseException {
        int len = selections.size();
        for ( int i = 0; i<len; i++ ) {
            String s = ( String ) selections.elementAt( i );
            out.cnfBeginFormula( s );
            if ( s.startsWith( "-" ) )
                out.append( "" + - variable.findNumber( s.substring( 1 ) ) );
            else
                out.append( ""+ variable.findNumber( s ) );
            out.endFormula();
        }
    }
}
