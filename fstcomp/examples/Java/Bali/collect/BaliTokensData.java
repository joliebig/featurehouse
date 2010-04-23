

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Bali token definitions are implemented as a {@link Map} of names (as
 * {@link String} objects) to their {@link String} definition.  Later
 * occurrences of a name override previous definitions.
 *
 * @layer<collect>
 */
    
public class BaliTokensData extends HashMap {

    public void addNode( BaliTokenDefineNode node ) {
        String name = ( String ) node.tok[1].getTokenName() ;
        String value = ( String ) node.tok[0].getTokenName() ;
        put( name, value ) ;
    }

}
