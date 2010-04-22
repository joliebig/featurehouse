

import java.util.*;
import java.io.*;

public class StatesDecl {

    public void add2Hash( Hashtable h, String source ) {

        // arg[0] is a AST_TypeNameList

        AstCursor outer = new  AstCursor();
        for ( outer.FirstElement( arg[0] ); outer.MoreElement(); 
              outer.NextElement() ) {

            // Step 1: foreach TypeName on the TypeName list, 
            //         get its name and see if it is in the hash table
            //         already.  If so, report an error.  Lastly,
            //         add the (name,source) to the list.

            TName t = ( TName ) outer.node;
            String n = t.GetName();
            defineState( h, n, source );
        }
    }
}
