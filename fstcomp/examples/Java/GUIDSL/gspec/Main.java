class Main    {
   // dump key tables
   static void debugActions() {
      variable.dumpVtable();
      production.dumpPtable();
      pattern.dumpTtable();
      original();
   }
}
