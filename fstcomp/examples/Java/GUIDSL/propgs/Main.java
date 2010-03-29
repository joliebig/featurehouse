// extending the static process method of Main adds the
// tasks to do for this layer, which is to propagate reference
// constraints on productions

import Jakarta.util.*;

class Main  {
   static public void process( Model m ) throws SemanticException {
      original(m);
      grammar.current.visit( new propcons() );
      if (Util.errorCount() !=0)
         throw new SemanticException("Errors in propagating Constraints");
   }
}
