

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

//--------------------------- Begin TOOL OUTPUT ------------------------
// the output of this tool is an instance of MMOutput.
// each MMOutput instance describes a unit of a collective.  By nesting 
// (or creating a hierarchy) of MMOutput instances, one can model 
// collectives of collectives.

// this is a variant of HashMap -- instead of overriding existing
// objects, copies are not stored.  This turns out to be important
// in Mixin-produced files, as the same method (unit) may appear
// in multiple places -- the first one counts.

public class MMHashMap extends HashMap {

    public void putUnique( String key,  MMOutput value ) {
        if ( !containsKey( key ) )
            super.put( key,value );
    }

    public void print( String indent ) { // for debugging

        List keys = new ArrayList( keySet() ) ;
        Collections.sort( keys ) ;

        for ( Iterator p = keys.iterator() ; p.hasNext() ; ) {
            MMOutput value = ( MMOutput ) get( ( String )p.next() ) ;
            value.print( indent +  MMGlobals.INDENT ) ;
        }
    }

}
