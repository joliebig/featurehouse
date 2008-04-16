// extension of the visitor that contains gui-specific code

class enterGspec {
   public void processOptid( ) {
      if (opt.equals("eqn")) {
         currentVar.eqn = true;
            foundOpt = true;
      }
      if (opt.equals("reverse")) {
         currentVar.reverse = true;
            foundOpt = true;
      }
        original();
   }

   public void processStrlit() {
      if (opt.equals("out")) {
         currentVar.out = optVal.substring(1,optVal.length()-1);
            foundOpt = true;
            return;
      }
        original();
   }
}
