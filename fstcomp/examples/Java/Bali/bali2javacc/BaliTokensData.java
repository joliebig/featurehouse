

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import java.util.logging.Level;

public class BaliTokensData {

    /**
     * Formats the Bali tokens as a JavaCC regular expression formed by
     * the union of strings, each a token listed on a separate line.
     *
     * @layer<bali2javacc>
     */
    public String toString() {

        if ( size() < 1 )
            return "// No Bali tokens defined in Bali grammar." ;

        List keys = new ArrayList( keySet() ) ;
        Collections.sort( keys ) ;

        StringBuffer buffer = new StringBuffer() ;
        buffer.append( "TOKEN : {" ) ;
        buffer.append( Main.LINE_SEPARATOR ) ;

        Iterator p = keys.iterator() ;
        if ( p.hasNext() ) {
            String key = ( String ) p.next() ;
            String value = ( String ) get( key ) ;
            buffer.append( "    <" + key + ": " + value + '>' ) ;
            buffer.append( Main.LINE_SEPARATOR ) ;
        }

        while ( p.hasNext() ) {
            String key = ( String ) p.next() ;
            String value = ( String ) get( key ) ;
            buffer.append( "    | <" + key + ": " + value + '>' ) ;
            buffer.append( Main.LINE_SEPARATOR ) ;
        }

        buffer.append( '}' ) ;
        return buffer.toString() ;
    }

}
