abstract class node {

    // cnf file translation for most nodes throws an exception.

    public void toCnfFormat( cnfout out ) throws CNFException {
        throw new CNFException( this.getClass().getName() +".toCnfFormat() invoked" );
    }
}
