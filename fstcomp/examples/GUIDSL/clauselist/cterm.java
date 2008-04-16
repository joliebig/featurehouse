import Jakarta.util.*;

// a cterm is a variable or its negation

public class cterm {
   public boolean negated;
   public variable var;

   cterm( boolean n ) {
      negated = n;
      var = null;
   }

   void setVar( variable v ) { var = v; }

   int eval3() {
      if (negated) return negate3(var.value);
      else return var.value;
   }

   int negate3( int v ) {
      switch (v) {
      case variable.T: return variable.F;
      case variable.F: return variable.T;
      case variable.U: return variable.U;
      }
      Util.fatalError("Unknown value: " + v );
      return -3;  // will never get here
   }

   void print() {
      if (negated) System.out.print(" -" + var.name);
      else System.out.print(" " + var.name);
   }
}
