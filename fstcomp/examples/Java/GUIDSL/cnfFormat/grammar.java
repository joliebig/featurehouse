class grammar {

    // generates root = true

    public static void toCnfFormat( cnfout out ) {
        out.cnfBeginFormula( rootProduction.var.name );
        out.append( rootProduction.var.number + "");
        out.endFormula();
    }
}
