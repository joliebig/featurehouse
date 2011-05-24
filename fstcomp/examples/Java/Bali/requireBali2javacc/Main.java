

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

/**
 * Extends {@link Main#driver(String[])} to generate warning and error
 * messages about non-terminals in the grammar.
 *
 * @layer<requireBali2javacc>
 */
    
public class Main {

    public Object driver( String[] args ) throws Throwable {

        setVersion( "v2002.08.27" ) ;

        Collector collector =
                ( Collector ) original( args ) ;

        List messages = new ArrayList( unDefined( collector.baliRules ) ) ;
        boolean hasErrors = ( ! messages.isEmpty() ) ;

        messages.addAll( misRequired( collector.baliRules ) ) ;
        messages.addAll( unReferenced( collector.baliRules ) ) ;

        Collections.sort( messages ) ;
        for ( Iterator p = messages.iterator() ; p.hasNext() ; )
            System.err.println( ( String ) p.next() ) ;

        if ( hasErrors )
            throw new IllegalStateException( "undefined rules are referenced in grammar" ) ;

        return collector ;
    }

    protected List misRequired( BaliRulesData rules ) {

        List list = rules.misRequired() ;
        for ( ListIterator p = list.listIterator() ; p.hasNext() ; ) {
            String rule = ( String ) p.next() ;
            p.set( "rule \""
                            + rule
                            + "\" is in a \"require\" statement, but is undefined" ) ;
        }

        return list ;
    }

    protected List unDefined( BaliRulesData rules ) {

        List list = rules.misDefined() ;
        for ( ListIterator p = list.listIterator() ; p.hasNext() ; ) {
            String rule = ( String ) p.next() ;
            p.set( "rule \""
                            + rule
                            + "\" is referenced but not defined" ) ;
        }

        return list ;
    }

    protected List unReferenced( BaliRulesData rules ) {

        List list = rules.unReferenced() ;
        for ( ListIterator p = list.listIterator() ; p.hasNext() ; ) {
            String rule = ( String ) p.next() ;
            p.set( "rule \""
                            + rule
                            + "\" is defined but not referenced" ) ;
        }

        return list ;
    }

}
