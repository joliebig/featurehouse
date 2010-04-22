

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class Es {
    public void commonAction( String mth ) {
        // Step 1: get the name of the state or edge

        String name = ( ( QName ) arg[0] ).GetName();
        boolean edge = ( mth.equals( "Action" ) || mth.equals( "Condition" ) );
        if ( edge )
            name = name + ":";

        // Step 2: find the declaration.  Add it if it is not
        //         already present

        MMOutput s = ( MMOutput )  Main.mmresult.nested.get( name );
        if ( s == null ) {
            if ( edge )
                s =  TransitionDecl.addTrans( name, false, this ); // its a transition
            else
                s =  StatesClause.add( name, false, this ); // its a state
        }

        // Step 2: find the method, and set its type to refines

        MMOutput m = ( MMOutput ) s.nested.get( mth );
        m.setDefn( MMGlobals.Refines );
    }
}
