

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

/**
 * Provides methods to generate source code for a method.  Inherits from
 * {@link CodeBuffer} to implement code body generation.
 *
 */
    
public class MethodBuilder extends  CodeBuffer {

    public  MethodBuilder addModifier( Modifier modifier ) {
        modifiers.add( modifier ) ;
        return this ;
    }

    public  MethodBuilder addParameter( String name, String type ) {
        parameters.put( name, new TypedName( name, type ) ) ;
        return this ;
    }

    public  MethodBuilder addThrowable( String name ) {
        throwables.add( name ) ;
        return this ;
    }

    public  CodeBuffer clear() {
        super.clear() ;
        methodReturn = null ;
        modifiers.clear() ;
        parameters.clear() ;
        throwables.clear() ;
        return this ;
    }

    public String getSignature() {

        StringBuffer buffer = new StringBuffer( methodReturn.getName() ) ;

        buffer.append( '(' ) ;
        if ( parameters.size() > 0 ) {
            Iterator p = parameters.values().iterator() ;
            buffer.append( ( ( TypedName ) p.next() ) . getType() ) ;
            while ( p.hasNext() ) {
                TypedName parameter = ( TypedName ) p.next() ;
                buffer.append( ',' ).append( parameter.getType() ) ;
            }
        }
        buffer.append( ')' ) ;

        return buffer.toString() ;
    }

    public  MethodBuilder removeModifier( Modifier modifier ) {
        modifiers.remove( modifier ) ;
        return this ;
    }

    public  MethodBuilder removeParameter( String name ) {
        parameters.remove( name ) ;
        return this ;
    }

    public  MethodBuilder removeThrowable( String name ) {
        throwables.remove( name ) ;
        return this ;
    }

    /**
     * Specifies a name for a constructor method.  This is equivalent to
     * <code>setReturn(name,name)</code>.
     *
     */
    public  MethodBuilder setConstructor( String name ) {
        return setReturn( name, name ) ;
    }

    /**
     * Sets the method name and return type for this method.  If the name
     * is the same as the return type, then the method is a constructor.
     *
     */
    public  MethodBuilder setReturn( String name, String type ) {
        methodReturn = new TypedName( name, type ) ;
        return this ;
    }

    public String toString() {

        // Begin method declaration with modifiers and return information:
        //
        CodeBuffer buffer = new CodeBuffer() ;
        for ( Iterator p = modifiers.iterator() ; p.hasNext() ; )
            buffer.append( p.next().toString() ) ;
        buffer.append( methodReturn.toString() ) ;

        // Build a phrase for the parameter list:
        //
        StringBuffer phrase = new StringBuffer() ;

        phrase.append( '(' ) ;
        if ( parameters.size() > 0 ) {
            Iterator p = parameters.values().iterator() ;
            phrase.append( p.next().toString() ) ;
            while ( p.hasNext() )
                phrase.append( ", " ).append( p.next().toString() ) ;
        }
        phrase.append( ')' ) ;

        // For long lines, each phrase starts on its own line:
        //
        if ( buffer.lineLength() + phrase.length() > LINE_LIMIT )
            buffer.endLine() ;
        buffer.append( phrase.toString() ) ;

        // Build a phrase for "throws" clause, if any "throws" present:
        //
        if ( throwables.size() > 0 ) {

            phrase.setLength( 0 ) ;
            phrase.append( "throws " ) ;
            Iterator p = throwables.iterator() ;
            phrase.append( p.next().toString() ) ;
            while ( p.hasNext() )
                phrase.append( ", " ).append( p.next().toString() ) ;

            if ( buffer.lineLength() + phrase.length() > LINE_LIMIT )
                buffer.endLine() ;
            buffer.append( phrase.toString() ) ;
        }

        buffer.append( "{" ).endLine() ;
        buffer.indent() ;
        buffer.appendLines( super.toString() ) ;
        buffer.endLine() ;
        buffer.outdent() ;
        buffer.append( "}" ) ;

        return buffer.toString() ;
    }

    //-------------------------------------------------------------------//
    // Private state information:
    //-------------------------------------------------------------------//

    private TypedName methodReturn ;

    final private Set modifiers = new TreeSet() ;
    final private Map parameters = new LinkedHashMap() ;
    final private Set throwables = new TreeSet() ;
    final private static class TypedName implements Comparable {

        TypedName( String name, String type ) {
            this.name = name.trim() ;
            this.type = type.trim() ;
            this.key = this.name + ':' + this.type ;
        }

        final public int compareTo( Object that ) {
            return key.compareTo( ( ( TypedName ) that ) . key ) ;
        }

        final public boolean equals( Object that ) {
            return key.equals( ( ( TypedName ) that ) . key ) ;
        }

        final public String getName() {
            return name ;
        }

        final public String getType() {
            return type ;
        }

        final public int hashCode() {
            return key.hashCode() ;
        }

        final public String toString() {
            return
                            ! name.equals( type )
		    ? type + ' ' + name
		    : name ;
        }

        final private String key ;
        final private String name ;
        final private String type ;

    }

}
