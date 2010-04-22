layer collect;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Regular expression token definitions are implemented as a {@link Map}
 * that maps each (states,kind) pair to a {@link List} of regular
 * expressions in declaration order.  Inner class {@link #Datum} holds the
 * parts of a regular expression token definition.
 *
 * @layer<collect>
 */
    
public class RegexTokensData extends HashMap {

    public void addNode( RegexDefinitionNode node ) {

        List states = statesList( ( StatesNode ) node.arg[0].arg [0] ) ;
        String kind = node.arg[1].tok[0].getTokenName() ;
        boolean noCase = ( node.arg[2].arg [0] != null ) ;

        List regexList =
                new ArrayList( ( ( AstList ) node.arg[3] ) . toList() ) ;

        Datum newDatum = new Datum( states, kind, noCase, regexList ) ;
        String key = newDatum.getKey() ;
        if ( ! containsKey( key ) ) {
            put( key, newDatum ) ;
            return ;
        }

        Datum oldDatum = ( Datum ) get( key ) ;
        oldDatum.regexList.addAll( regexList ) ;
    }

    /**
     * Formats regular expression tokens by concatenating them with blank
     * line separators.
     *
     * @layer<collect>
     */
    public String toString() {

        if ( size() < 1 )
            return "// No regular expression tokens in Bali grammar." ;

        List keys = new ArrayList( keySet() ) ;
        Collections.sort( keys ) ;

        StringBuffer buffer = new StringBuffer() ;

        for ( Iterator p = keys.iterator() ; p.hasNext() ; ) {
            Datum value = ( Datum ) get( p.next() ) ;
            buffer.append( value ).append( Main.LINE_SEPARATOR ) ;
        }

        return buffer.toString() ;
    }
    static protected class Datum {

        public Datum( List states, String kind, boolean noCase, List re ) {

            this.states = states ;
            this.kind = kind ;
            this.noCase = noCase ;
            this.regexList = re ;

            this.key = keyToString() ;
        }

        public String getKey() {
            return key ;
        }

        public String keyToString() {

            StringBuffer buffer = new StringBuffer() ;

            if ( states.size() > 0 ) {
                buffer.append( '<' ) ;

                Iterator p = states.iterator() ;
                buffer.append( p.next().toString() ) ;
                while ( p.hasNext() )
                    buffer.append( ',' ).append( p.next().toString() ) ;

                buffer.append( "> " ) ;
            }

            buffer.append( kind ) ;

            if ( noCase )
                buffer.append( " IGNORE_CASE" ) ;

            return buffer.toString() ;
        }

        public String toString() {

            StringBuffer buffer = new StringBuffer( key ) ;

            buffer.append( ": {" ) ;

            Iterator p = regexList.iterator() ;
            buffer.append( p.next().toString() ) ;
            while ( p.hasNext() )
                buffer.append( " | " ).append( p.next().toString() ) ;

            buffer.append( Main.LINE_SEPARATOR ).append( '}' ) ;
            return buffer.toString() ;
        }

        final protected String key ;
        final protected String kind ;
        final protected boolean noCase ;
        final protected List regexList ;
        final protected List states ;
    }

    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    /**
     * Returns the list of states for this regular expression token as a
     * {@link List} of {@link String} objects naming the states.  An empty
     * {@link List} means *all* states.
     *
     * @layer<collect>
     */
    final private List statesList( StatesNode node ) {

        if ( node == null )
            return Collections.EMPTY_LIST ;

        StatesSpecifier subNode = ( StatesSpecifier ) node.arg[0] ;
        if ( subNode instanceof  StarStatesNode )
            return Collections.EMPTY_LIST ;

        Main.DEBUG.info( "subNode.class="
                + subNode.getClass()
                + "; subNode="
                + subNode ) ;

        List states = new ArrayList() ;

        Iterator p = ( ( AstList )subNode.arg[0] ).toList().iterator() ;
        while ( p.hasNext() ) {
            StateNameNode nameNode = ( StateNameNode ) p.next() ;
            states.add( nameNode.tok[0].getTokenName() ) ;
        }

        return Collections.unmodifiableList( Arrays.asList( states.toArray( new String [states.size()] ) ) ) ;
    }

}
