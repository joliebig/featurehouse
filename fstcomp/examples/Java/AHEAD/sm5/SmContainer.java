

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// SmContainer is an unusual container in that it is highly integrated
// with sdInfo objects.  There is a pair of SmContainers per sdInfo
// object -- one for states, another for transitions.  The thing that is 
// unusual is that SmContainers allow you to reference any object
// (state or transition) in that SM declaration, and any object (state or
// transition) in the parent SM declarations in an inheritance hierarchy.
// all the code for SmContainer largely deals with hiding the fact
// that we are forming the union of a set of lower-level containers
// (implemented as PrintableVectors) and doing so in a seamless way.

class SmContainer implements Serializable {
    sdInfo Smlocal;
    boolean contains_states;
    public PrintableVector v;

    public SmContainer( sdInfo loc, boolean has_states ) {
        Smlocal         = loc;
        contains_states = has_states;
        v               = new PrintableVector();
    }
   
    public SmIterator iterator() {
        return new SmIterator( Smlocal, contains_states );
    }
   
    public Object find( Object j ) {
        Object o;

        SmIterator i = this.iterator();
        for ( o = i.firstObj();
               o != null;
               o = i.nextObj() )

            if ( o.equals( j ) )
                return o;
        return null;
    }

    public void print() {
        SmIterator i = this.iterator();
        for ( Object o = i.firstObj(); o != null; o = i.nextObj() )
            ( ( printTruncObject ) o ).print();
    }

    // add object to container

    public boolean add( Object o ) {
        return v.add( o );
    }

    // truncate all objects in the container

    public void truncate() {
        SmIterator ii = iterator();
        printTruncObject e;

        for ( e = ( printTruncObject ) ii.firstObj(); 
               e != null; 
               e = ( printTruncObject ) ii.nextObj() )
            e.truncate();
    }
}
