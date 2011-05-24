
class BImplies {

    public node eharvest () {
      OExpr left = (OExpr) arg[0];
		IExpr right = (IExpr) arg[1];
		return new implies( left.eharvest(), right.eharvest() );
    }

}
