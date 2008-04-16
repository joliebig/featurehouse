class bterm {

    // print number of variable

    public void toCnfFormat( cnfout out ) throws CNFException {
        try {
		     out.print( variable.findNumber( name ) + " " );
		  }
		  catch (Exception e) { throw new CNFException( e.getMessage() ); }
    }
}
