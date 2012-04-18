

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Options blocks are implemented as a {@link List} of {@link String}
 * objects representing the collected code.  They are stored in order of
 * declaration.
 *
 * @layer<collect>
 */
    
public class OptionBlocksData extends ArrayList {

    public void addNode( OptionsNode node ) {
        String block = node.arg[0].arg[0].tok[0].getTokenName() ;
        add( block ) ;
    }

    /**
     * Formats options blocks by concatenating them with blank line
     * separators and with "options {" prefix and "} options" suffix.
     *
     * @layer<collect>
     */
    public String toString() {

        if ( size() < 1 )
            return "// No options blocks in Bali grammar." ;

        StringBuffer buffer = new StringBuffer( "options {" ) ;

        for ( Iterator p = iterator() ; p.hasNext() ; )
            buffer.append( p.next() ).append( Main.LINE_SEPARATOR ) ;

        buffer.append( "} options" ) ;
        return buffer.toString() ;
    }

}
