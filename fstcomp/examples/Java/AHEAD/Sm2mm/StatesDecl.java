

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class StatesDecl {
    public void execute( int stage ) {
        if ( stage!=0 ) {
            super.execute( stage );
            return;
        }

        // Step 1: harvest each state name
  
        MMOutput m =  Main.mmresult;
        AstCursor c = new  AstCursor();
        for ( c.FirstElement( arg[0] ); c.MoreElement(); c.NextElement() ) {
            TName t = ( ( TName ) c.node );
            String name = t.GetName();

            // Step 2: foreach name, create an MMOutput and insert it into
            //         the main object.  

            add( name, true, this );
        }
    }
}
