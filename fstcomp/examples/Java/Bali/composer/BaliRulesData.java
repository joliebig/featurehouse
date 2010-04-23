

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.logging.Level;

//-----------------------------------------------------------------------//
// Composer-specific collections from the parse tree:
// (adds composer-specific {@link Object#toString()} methods)
//-----------------------------------------------------------------------//

public class BaliRulesData {

    /**
     * Formats Bali rules by concatenating them with blank line separation.
     *
     * @layer<composer>
     */
    public String toString() {

        List keys = new ArrayList( keySet() ) ;

        StringBuffer buffer = new StringBuffer() ;
        bufferRule( buffer, getStartName() ) ;
        keys.remove( getStartName() ) ;
            
        Collections.sort( keys ) ;
        for ( Iterator p = keys.iterator() ; p.hasNext() ; ) {
            buffer.append( Main.LINE_SEPARATOR ) ;
            buffer.append( Main.LINE_SEPARATOR ) ;
            bufferRule( buffer, ( String ) p.next() ) ;
        }

        return buffer.toString() ;
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

    private void bufferRule( StringBuffer buffer, String name ) {

        buffer.append( name ) ;

        List originals = ( List ) get( name ) ;
        List productions = new ArrayList( originals.size() ) ;
        HashMap namedRuleMap = new HashMap() ;
        for ( Iterator p = originals.iterator() ; p.hasNext() ; ) {
            Production production = ( Production ) p.next() ;
            ProductionVisitor v = new  ProductionVisitor() ;
            v.dispatch( production ) ;
            String ruleName = v.getRuleName() ;
            if ( ruleName == null ) {
                productions.add( production ) ;
                continue ;
            }
            if ( namedRuleMap.get( ruleName ) == null ) {
                productions.add( production ) ;
                namedRuleMap.put( ruleName, production ) ;
            }
        }

        char separator = ':' ;
        for ( Iterator p = productions.iterator() ; p.hasNext() ; ) {
            buffer.append( Main.LINE_SEPARATOR ) ;
            buffer.append( '\t' ) ;
            buffer.append( separator ) ;
            buffer.append( p.next() ) ;
            separator = '|' ;
        }

        buffer.append( Main.LINE_SEPARATOR ) ;
        buffer.append( '\t' ) ;
        buffer.append( ';' ) ;
    }

}
