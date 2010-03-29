class not {

    // negation indicated by minus

    public void toCnfFormat( cnfout out ) throws CNFException {
        out.print( "-" );
        left.toCnfFormat( out );
    }
}
