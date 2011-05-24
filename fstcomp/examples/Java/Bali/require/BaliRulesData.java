

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Keeps track of rule "require" statements, definitions and references.
 *
 * @layer<require>
 */
    
public class BaliRulesData {

    final public static int DEFINED = 1 ;
    final public static int REFERENCED = 2 ;
    final public static int REQUIRED = 4 ;

    final public static int MASK = DEFINED | REFERENCED | REQUIRED ;

    /**
     * A rule name is defined when it's on the left-hand side of a rule.
     *
     * @layer<require>
     */
    public void addDefinition( String name ) {
        setBits( name, DEFINED ) ;
    }

    /**
     * A rule name is referenced when it's used in the right-hand side of a
     * rule.
     *
     * @layer<require>
     */
    public void addReference( String name ) {
        setBits( name, REFERENCED ) ;
    }

    /**
     * A rule name is required when it's in a "require" statement.
     *
     * @layer<require>
     */
    public void addRequire( String name ) {
        setBits( name, REQUIRED ) ;
    }

    /**
     * Return a {@link List} of those names that include given status bits.
     *
     * @layer<require>
     */
    public List getList( int mask, int status ) {

        List list = new ArrayList() ;
        for ( Iterator p = usageMap.keySet().iterator() ; p.hasNext() ; ) {
            String key = ( String ) p.next() ;
            if ( ( mask & getStatus( key ) ) == status )
                list.add( key ) ;
        }

        return Arrays.asList( list.toArray( new String [list.size()] ) ) ;
    }

    /**
     * Return the status bits associated with a given name.
     *
     * @layer<require>
     */
    public int getStatus( String name ) {
        Integer status = ( Integer ) usageMap.get( name ) ;
        return ( status != null ) ? ( MASK & status.intValue() ) : 0 ;
    }

    /**
     * Returns rules that are referenced but neither defined nor in a
     * "require" statement.
     *
     * @layer<require>
     */
    public List misDefined() {
        return getList( DEFINED | REFERENCED | REQUIRED, REFERENCED ) ;
    }

    /**
     * Specifies the name of the start rule (an implicit reference).
     *
     * @layer<require>
     */
    protected void setStartName( String name ) {
        original( name ) ;
        addReference( name ) ;
    }

    /**
     * Include bits in the status of a rule name.
     *
     * @layer<require>
     */
    protected void setBits( String name, int bits ) {
        usageMap.put( name, STATUS [ ( MASK & bits ) | getStatus( name )] ) ;
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

    final private Map usageMap = new HashMap() ;

    final private static Integer[] STATUS = new Integer[] {
        null,
        new Integer( 1 ),
        new Integer( 2 ),
        new Integer( 3 ),
        new Integer( 4 ),
        new Integer( 5 ),
        new Integer( 6 ),
        new Integer( 7 ),
	} ;

}
