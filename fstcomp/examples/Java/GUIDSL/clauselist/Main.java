// generate cnfClauses for each formula

import Jakarta.util.*;

class Main  {
   static public void process( Model m ) throws SemanticException {
      original(m);
      production.makeClauses();
      pattern.makeClauses();
      ESList.makeClauses();
      grammar.makeClauses();
      if (Util.errorCount() != 0)
         throw new SemanticException( "Errors in making conjunctive normal formulas" );
   }
}
