

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * Extends {@link Main#driver(String[])} to generate warning messages about
 * referenced rules neither undefined nor in "require" statements.
 *
 * @layer<requireBali2jak>
 */
    
public class Main {

    public Object driver( String[] args ) throws Throwable {

        setVersion( "v2002.08.27" ) ;

        Collector collector =
                ( Collector ) original( args ) ;

        List misDefined = collector.baliRules.misDefined() ;
        Collections.sort( misDefined ) ;
        for ( Iterator p = misDefined.iterator() ; p.hasNext() ; ) {
            String rule = ( String ) p.next() ;
            System.err.println( "rule \"" + rule + "\" is referenced but not defined" ) ;
        }

        return collector ;
    }

}
