layer collect;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Java code blocks are implemented as a {@link List} of {@link String}
 * objects representing the collected code.  They are stored in order of
 * declaration.
 *
 * @layer<collect>
 */
    
public class JavaBlocksData extends ArrayList {

    public void addNode( JavacodeNode node ) {
        String prefix = node.arg[0].arg[0].tok[0].getTokenName() ;
        String suffix = node.arg[0].arg[1].tok[0].getTokenName() ;
        add( prefix + suffix ) ;
    }

    /**
     * Formats the Java code blocks, each beginning on a separate line and
     * separated from the others by a blank line, by placing a "JAVACODE"
     * prefix and a "}" suffix.
     *
     * @layer<collect>
     */
    public String toString() {

        if ( size() < 1 )
            return "// No JAVACODE blocks in Bali grammar." ;

        StringBuffer buffer = new StringBuffer() ;

        Iterator p = iterator() ;
        if ( p.hasNext() ) {
            buffer.append( "JAVACODE" ) ;
            buffer.append( p.next() ) ;
            buffer.append( Main.LINE_SEPARATOR ) ;
            buffer.append( '}' ) ;
        }

        while ( p.hasNext() ) {
            buffer.append( Main.LINE_SEPARATOR ) ;
            buffer.append( Main.LINE_SEPARATOR ) ;
            buffer.append( "JAVACODE" ) ;
            buffer.append( p.next() ) ;
            buffer.append( Main.LINE_SEPARATOR ) ;
            buffer.append( '}' ) ;
        }

        return buffer.toString() ;
    }

}
