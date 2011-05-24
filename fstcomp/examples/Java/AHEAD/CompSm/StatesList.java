

import java.util.*;
import java.io.*;

public class StatesList {

    public void compose( AstNode etree ) {

        // Step 1: composition of states list is simple.  just append
        //         the stateslist of the extension to the stateslist of
        //         the base

        StatesList e = ( StatesList ) etree;
        add( e );
    }

    public void add2Hash( Hashtable h, String source ) {

        // foreach StatesClause on a StatesList, extract the names of states
        // and add it to hash table h.  Note any replication errors.
   
        AstCursor c = new  AstCursor();
 
        for ( c.FirstElement( this ); c.MoreElement(); c.NextElement() ) {
            StatesClause sc = ( StatesClause ) c.node;
            sc.add2Hash( h, source );
        }
    }
}
