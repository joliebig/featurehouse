class grammar {

   static void makeClauses() {
      cnfClause c = new cnfClause();
      cterm  t = new cterm(false);
      t.setVar( rootProduction.var );
      c.add(t);
      c.setFormula(new bterm(rootProduction.var.name));
      cnfClause.clist.add(c);
   }
}
