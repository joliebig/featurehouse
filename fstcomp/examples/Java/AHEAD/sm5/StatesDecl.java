

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class StatesDecl {
    public void reduce2java( AstProperties props ) {
        AstCursor  c = new  AstCursor();
        String           stateName;
        stateInfo        s;

        for ( c.FirstElement( arg[0] ); c.MoreElement(); c.NextElement() ) {
            stateName = c.node.arg[0].arg[0].arg[0].arg[0].tok[0].tokenName();
            defineState( stateName, false, tok[0] );
        }
    }
}
