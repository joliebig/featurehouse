class and {

    boolean oktype( node n ) {
        return ( n instanceof not || n instanceof bterm || n instanceof or );
    }

    public void toCnfFormat( cnfout out ) throws CNFException {
        // convert the left argument -- add eol if something reasonable
        // sits below
 
        out.andSeen();

        left.toCnfFormat( out );
        if ( oktype( left ) ) {
            out.println( " 0" );
            out.inc();
        }

        // now do the same for the right...

        right.toCnfFormat( out );
        if ( oktype( right ) ) {
            out.println( " 0" );
            out.inc();
        }
    }
}
