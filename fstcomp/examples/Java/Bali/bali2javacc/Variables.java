

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

/**
 * Implements a <em>temporary variables pool</em>, providing methods to
 * retrieve free variables from the pool, reset the pool and generate
 * Java declarations for the variables.
 *
 * @layer<bali2javacc>
 */
    
public class Variables {

    final public String LOWER = "abcdefghijklmnopqrstuvwxyz" ;
    final public String UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;

    public Variables() {
        this.declarations = new ArrayList() ;
        this.prefixSet = new HashSet() ;
        this.random = new Random( 0L ) ;
        this.typeMap = new TreeMap() ;
        this.usedSet = new HashSet() ;
    }

    public String declare( String type, String name, String value ) {
        declarations.add( type + ' ' + name + " = " + value ) ;
        return name ;
    }

    public String getFree( String type ) {

        TypeData data = ( TypeData ) typeMap.get( type ) ;
        if ( data == null ) {
            data = new TypeData( type ) ;
            typeMap.put( type, data ) ;
        }
            
        String variable = data.prefix + data.count ;
        data.limit = Math.max( data.limit, ++data.count ) ;
        return variable ;
    }

    public String randomChars() {
        char one = UPPER.charAt( random.nextInt( UPPER.length() ) ) ;
        char two = LOWER.charAt( random.nextInt( LOWER.length() ) ) ;
        return new String( new char[] {one, two} ) ;
    }

    /**
     * Frees all the variables currently in use in this pool.
     *
     * @layer<bali2javacc>
     */
    public void reset() {

        for ( Iterator p = typeMap.values().iterator() ; p.hasNext() ; ) {
            TypeData data = ( TypeData ) p.next() ;
            data.count = 0 ;
        }
    }

    /**
     * Returns a String of Java declarations for the variables in the pool.
     *
     * @layer<bali2javacc>
     */
    public String toString() {

        StringBuffer buffer = new StringBuffer() ;
        Collections.sort( declarations ) ;
        for ( Iterator p = declarations.iterator() ; p.hasNext() ; ) {

            if ( buffer.length() > 0 )
                buffer.append( Main.LINE_SEPARATOR ) ;

            buffer.append( p.next().toString() ) ;
            buffer.append( " ;" ) ;
        }

        for ( Iterator p = typeMap.entrySet().iterator() ; p.hasNext() ; ) {

            Map.Entry entry = ( Map.Entry ) p.next() ;
            String type = ( String ) entry.getKey() ;
            TypeData data = ( TypeData ) entry.getValue() ;

            if ( buffer.length() > 0 )
                buffer.append( Main.LINE_SEPARATOR ) ;

            buffer.append( type ) ;
            buffer.append( ' ' ) ;
            for ( int n = data.limit ; --n > 0 ; )
                buffer.append( data.prefix + n + "=null, " ) ;
            buffer.append( data.prefix + "0=null ;" ) ;
        }

        return buffer.toString() ;
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

    final private List declarations ;
    final private Set prefixSet ;
    final private Random random ;
    final private Map typeMap ;
    final private Set usedSet ;
    final public class TypeData {

        TypeData( String type ) {

            String leader = type.substring( 0,2 ).toLowerCase() ;
            String prefix = leader ;
            while ( prefixSet.contains( prefix ) )
                prefix = leader + randomChars() ;
            prefixSet.add( prefix ) ;

            this.count = 0 ;
            this.limit = 0 ;
            this.prefix = prefix ;
        }

        public int count ;
        public int limit ;

        public String prefix ;

    }

}
