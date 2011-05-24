class BOr {

    public node eharvest () {
       AExpr left = (AExpr) arg[0];
		 OExpr right = (OExpr) arg[1];
		 return new or (left.eharvest(), right.eharvest());
    }

}
