class cterm {

   boolean eval2() {
      boolean val = (var.value == variable.T);
      if (negated) return !val ;
      else return val;
   }

}
