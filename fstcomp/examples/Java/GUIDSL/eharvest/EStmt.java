class EStmt {

    public node eharvest () {
	    Expr e = (Expr) arg[0];
		 node n = e.eharvest();
		 ESList.CTable.add(n);
		 return null;
    }

}
