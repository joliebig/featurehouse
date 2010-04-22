layer requireComposer;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/**
 * Modifies code generation for rules by generating "require" statements
 * for referenced rules that aren't defined.
 *
 * @layer<requireComposer>
 */
    
public refines class BaliRulesData {

    /**
     * Refined to set <code>REQUIRED</code> bit only if the given rule is
     * not already <code>DEFINED</code>.  This allows <code>REQUIRED</code>
     * statements to be erased by previous rule definitions.
     *
     * @layer<requireComposer>
     */
    public void addRequire( String name ) {
        if ( ( getStatus( name ) & DEFINED ) == 0 )
            setBits( name, REQUIRED ) ;
    }

    /**
     * Returns rules that are referenced but not defined.
     *
     * @layer<requireComposer>
     */
    public List misDefined() {
        return getList( DEFINED | REFERENCED | REQUIRED, REFERENCED ) ;
    }

    public String toString() {

        Set requireSet = new TreeSet( getList( REQUIRED, REQUIRED ) ) ;
        if ( requireSet.size() < 1 )
            return Super().toString() ;

        StringBuffer buffer = new StringBuffer() ;
        for ( Iterator p = requireSet.iterator() ; p.hasNext() ; ) {
            String rule = ( String ) p.next() ;
            String type = getType( rule ) ;
            buffer.append( "require "
                            + rule
                            + ( rule.equals( type ) ? "" : ( " -> " + type ) )
                            + " ;"
                            + Main.LINE_SEPARATOR ) ;
        }

        return buffer.toString() + Main.LINE_SEPARATOR + Super().toString() ;
    }

}
