class BIff {

    public node eharvest () {
       IExpr left = (IExpr) arg[0];
		 EExpr right = (EExpr) arg[1];
		 return new iff( left.eharvest(), right.eharvest() );
    }

}
