class BNot {

    public node eharvest () {
	    NExpr n = (NExpr) arg[0];
		 return new not( n.eharvest() );
    }

}
