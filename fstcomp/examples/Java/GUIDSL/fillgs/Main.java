// extending the static process method of Main adds the
// tasks to do for this layer

import Jakarta.util.*;

class Main  {
   static public void process( Model m ) throws SemanticException {
       original(m);
        // harvest the tree
        m.harvest( new fillFPtable() );
        if (Util.errorCount() != 0)
           throw new SemanticException( "Error(s) in specification found" );
        m.harvest( new enterGspec() );
        if (Util.errorCount() != 0)
           throw new SemanticException( "Error(s) in specification found" );
    }
}
