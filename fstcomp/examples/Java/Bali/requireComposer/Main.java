layer requireComposer;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/**
 * Extends {@link Main#driver(String[])} to generate warning messages
 * about referenced rules that are undefined.
 *
 * @layer<requireComposer>
 */
    
public refines class Main {

    public Object driver( String[] args ) throws Throwable {

        setVersion( "v2002.09.04" ) ;

        Collector collector =
                ( Collector ) Super( String[] ).driver( args ) ;

        List misDefined = collector.baliRules.misDefined() ;
        Collections.sort( misDefined ) ;
        for ( Iterator p = misDefined.iterator() ; p.hasNext() ; ) {
            String rule = ( String ) p.next() ;
            System.err.println( "rule \"" + rule + "\" is referenced but not defined" ) ;
        }

        return collector ;
    }

}
