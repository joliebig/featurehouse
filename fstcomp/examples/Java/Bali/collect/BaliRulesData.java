

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// Classes for data collected from parse trees:
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

/**
 * Contains a {@link Map} that maps each rule name to a {@link List} of
 * productions, each represented by a {@link ProductionNode}.  Also keeps
 * the name of the start rule.
 *
 * @see #getStartName()
 *
 * @layer<collect>
 */
    
public class BaliRulesData extends HashMap {

    /**
     * Adds the rulename and productions from a given
     * {@link BaliGrammarNode}.  The starting rule name is taken from the
     * first one {@link BaliGrammarNode} added.
     *
     * @layer<collect>
     */
    public void addNode( BaliGrammarNode node ) {

        String name = node.tok[0].getTokenName() ;
        if ( size() < 1 )
            setStartName( name ) ;

        List productions = ( List ) get( name ) ;
        if ( productions == null ) {
            productions = new ArrayList() ;
            put( name, productions ) ;
        }

        AstList astList = ( AstList ) ( node.arg [0] ) ;
        productions.addAll( 0, astList.toList() ) ;
    }

    /**
     * Returns the name of the start rule.  May be <code>null</code> if not
     * yet defined.
     *
     * @layer<collect>
     */
    public String getStartName() {
        return startName ;
    }

    /**
     * Specifies the name of the start rule.
     *
     * @layer<collect>
     */
    protected void setStartName( String name ) {
        if ( startName != null )
            throw new IllegalStateException( "start symbol redefinition" ) ;
        startName = name ;
    }

    /**
     * Returns the type associated with a given rule name.
     *
     * @layer<collect>
     */
    public String getType( String name ) {
        String type = ( String ) typeMap.get( name ) ;
        return ( type != null ) ? type : name ;
    }

    /**
     * Specifies a type for a given rule name.
     *
     * @layer<collect>
     */
    public void setType( String name, String type ) {
        if ( name != null && type != null )
            typeMap.put( name, type ) ;
    }

    /**
     * Extends <code>toString</code> by appending start rule information.
     *
     * @layer<collect>
     */
    public String toString() {
        return
                super.toString().replaceAll( "\\s+", " " )
                + " / start@"
                + getStartName() ;
    }

    private String startName = null ;
    final private Map typeMap = new HashMap() ;
}
