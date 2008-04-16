class VarDef {

    public node eharvest () {
       // Step 1: define variable first

       String name = tok[1].getTokenName();
		 variable.define( name, variable.Prim, null, false );

		 // Step 2: now define expression

       Expr e = (Expr) arg[0];
		 node n = e.eharvest();
		 ESList.CTable.add(new iff( new bterm(name), n));
		 return null;
    }

}
