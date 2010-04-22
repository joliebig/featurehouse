layer codegen;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

//-----------------------------------------------------------------------//
// Builder classes for code generation:
//-----------------------------------------------------------------------//

/**
 * Provides an interface analogous to {@link StringBuffer}, though not as
 * complete, that is tailored to producing lines of source code.
 *
 * @layer<codegen>
 */
    
public class CodeBuffer {

    public  CodeBuffer append( char character ) {
        return append( String.valueOf( character ) ) ;
    }

    public  CodeBuffer append( List tokensInPhrase ) {

        Iterator p = tokensInPhrase.iterator() ;
        StringBuffer buffer = new StringBuffer( p.next().toString() ) ;
        while ( p.hasNext() )
            buffer.append( SPACE ).append( p.next() ) ;

        return append( buffer.toString() ) ;
    }

    public  CodeBuffer append( String string ) {

        int spaces = hasSeparation( string ) ? 0 : 1 ;

        if ( ! beginLine() )
            if ( lineLength() + string.length() + spaces > LINE_LIMIT )
                endLine() ;
            
        if ( beginLine() )
            buffer.append( indentation ) ;

        if ( ! hasSeparation() )
            buffer.append( SPACE ) ;

        buffer.append( string ) ;
        return this ;
    }

    public  CodeBuffer append( StringEnumeration value ) {
        return append( value.toString() ) ;
    }

    public  CodeBuffer appendLines( String text ) {

        List lines = Arrays.asList( EOL_PATTERN.split( text, -1 ) ) ;
        if ( lines.size() > 0 ) {
                        
            int leader = leadingSpaces( lines ) ;

            Iterator p = lines.iterator() ;
            append( ( ( String ) p.next() ) . substring( leader ) ) ;
            while ( p.hasNext() ) {
                String line = ( String ) p.next() ;
                endLine() ;
                append( line.substring( leader ) ) ;
            }
        }

        return this ;
    }

    public  CodeBuffer clear() {
        buffer.setLength( 0 ) ;
        return this ;
    }

    public  CodeBuffer endLine() {
        buffer.append( Main.LINE_SEPARATOR ) ;
        return this ;
    }

    public  CodeBuffer indent( String indentString ) {
        indentation.append( indentString ) ;
        return this ;
    }

    public  CodeBuffer indent() {
        return indent( INDENTATION ) ;
    }

    public int length() {
        return buffer.length() ;
    }

    final public int lineLength() {
        int index = buffer.lastIndexOf( Main.LINE_SEPARATOR ) ;
        return
                ( index >= 0 )
		? buffer.length() - index + Main.LINE_SEPARATOR.length()
		: buffer.length() ;
    }

    public  CodeBuffer outdent( String indentString ) {

        int index = indentation.length() - indentString.length() ;
        String previous = indentation.substring( index ) ;
        if ( ! previous.equals( indentString ) )
            throw new IllegalArgumentException( "outdent != indent" ) ;

        indentation.delete( index, indentation.length() ) ;
        return this ;
    }

    public  CodeBuffer outdent() {
        return outdent( INDENTATION ) ;
    }

    public int size() {
        return buffer.length() ;
    }

    public  CodeBuffer spaceToColumn( int column ) {
        for ( int spaces = column - lineLength() ; --spaces >= 0 ; )
            buffer.append( SPACE ) ;
        return this ;
    }

    public String toString() {
        return buffer.toString() ;
    }

    //-------------------------------------------------------------------//

    final private boolean beginLine() {
        int index = buffer.length() - Main.LINE_SEPARATOR.length() ;
        return
                index >= 0
                && Main.LINE_SEPARATOR.equals( buffer.substring( index ) ) ;
    }

    final private static int leadingSpaces( List strings ) {

        Iterator p = strings.iterator() ;
        if ( ! p.hasNext() )
            return 0 ;
            
        int lead = leadingSpaces( ( String ) p.next() ) ;
        while ( p.hasNext() )
            lead = Math.min( lead, leadingSpaces( ( String ) p.next() ) ) ;

        return lead ;
    }

    final private boolean hasSeparation() {
        return
                buffer.length() < 1
                || Character.isWhitespace( buffer.charAt( buffer.length()-1 ) ) ;
    }

    final private boolean hasSeparation( String text ) {
        return
                hasSeparation()
                || ( text.length() > 0
                    && Character.isWhitespace( text.charAt( 0 ) ) ) ;
    }

    final private static int leadingSpaces( String string ) {

        for ( int n = 0 ; n < string.length() ; ++n )
            if ( string.charAt( n ) != SPACE )
                return n ;

        return string.length() ;
    }

    final private StringBuffer buffer = new StringBuffer() ;
    final private StringBuffer indentation = new StringBuffer() ;

    final private static java.util.regex.Pattern
        EOL_PATTERN = java.util.regex.Pattern.compile( "\\r*\\n\\r*|\\n*\\r\\n*" ) ;

    final private static String
        INDENTATION = "    " ;

    final public static int
        LINE_LIMIT = 78 ;

    final private static char
        SPACE = ' ' ;

}
