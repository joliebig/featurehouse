// extending the static process method of Main
// allows us to generate a formula for each production
// and each pattern.  The conjunction of these formulas
// is the formula that is represented by the grammar,
// sans 'root=true'

import Jakarta.util.*;

class Main  {
   static public void process( Model m ) throws SemanticException {
      original(m);
      production.makeFormula();
      pattern.makeFormula();
      if (Util.errorCount() != 0)
         throw new SemanticException( "Errors in making propositional formulas" );
   }
}
