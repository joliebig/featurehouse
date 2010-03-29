class BAnd {

    public node eharvest () {
	    NExpr left = (NExpr) arg[0];
	    AExpr right = (AExpr) arg[1];
       return new and( left.eharvest(), right.eharvest() );
    }
}
