class or {

    public void toCnfFormat( cnfout out ) throws CNFException {
        left.toCnfFormat( out );
        right.toCnfFormat( out );
    }
}
