

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
 * Provides a <em>builder</em> class to construct Jak source classes.
 * A <code>ClassBuilder</code> class can be used to build multiple classes
 * by either re-using state information for multiple classes or by invoking
 * {@link ClassBuilder#initialize()} between class constructions.
 *
 * @layer<codegen>
 */
    
public class ClassBuilder extends  CodeBuffer {

    /**
     * Clears the state information about the current class being
     * constructed so that a new class can be built.
     *
     * @layer<codegen>
     */
    public  CodeBuffer clear() {
        layerName = null ;
        className = null ;
        superName = null ;
	imports.clear () ;
        interfaces.clear() ;
        methods.clear() ;
        modifiers.clear() ;
        return this ;
    }

    /**
     * Adds a package/class to the imports list for this class.
     *
     * @layer<codegen>
     **/
    public ClassBuilder addImport (String name) {
	imports.add (name) ;
	return this ;
    }

    /**
     * Removes an import from the referenced imports for this class.
     *
     * @layer<codegen>
     */
    public  ClassBuilder removeImport( String name ) {
        imports.remove( name ) ;
        return this ;
    }

    /**
     * Adds an interface to the implemented interfaces for this class.
     *
     * @layer<codegen>
     */
    public  ClassBuilder addInterface( String name ) {
        interfaces.add( name ) ;
        return this ;
    }

    /**
     * Removes an interface from the implemented interfaces for this class.
     *
     * @layer<codegen>
     */
    public  ClassBuilder removeInterface( String name ) {
        interfaces.remove( name ) ;
        return this ;
    }

    /**
     * Adds a method to the class by extracting it from the contents of
     * a given {@link MethodBuilder} instance.
     *
     * @layer<codegen>
     */
    public  ClassBuilder addMethod( MethodBuilder method ) {
        addMethod( method.getSignature(), method.toString() ) ;
        return this ;
    }

    /**
     * Adds a method signature and body to be included in the generated
     * class.
     *
     * @layer<codegen>
     */
    public  ClassBuilder addMethod( String signature, String method ) {
        methods.put( signature, method ) ;
        return this ;
    }

    /**
     * Removes a method from the generated class.
     *
     * @layer<codegen>
     */
    public  ClassBuilder removeMethod( String signature ) {
        methods.remove( signature ) ;
        return this ;
    }

    /**
     * Adds a class modifier to the class to be generated.
     *
     * @layer<codegen>
     */
    public  ClassBuilder addModifier( Modifier modifier ) {

        if ( modifier == null )
            throw new IllegalArgumentException( "\"modifier\" is null" ) ;

        if ( modifier.equals( Modifier.REFINES ) && superName != null )
            throw new IllegalArgumentException( "invalid w/ super class" ) ;

        modifiers.add( modifier ) ;
        return this ;
    }

    /**
     * Removes a class modifier from the class.
     *
     * @layer<codegen>
     */
    public  ClassBuilder removeModifier( Modifier modifier ) {
        modifiers.remove( modifier ) ;
        return this ;
    }

    /**
     * Sets the layer name, if <code>name</code> is non-null, or clears
     * the layer name if <code>name==null</code>.
     *
     * @layer<codegen>
     */
    public  ClassBuilder setLayerName( String name ) {
        layerName = name ;
        return this ;
    }

    /**
     * Sets the class name, if <code>name</code> is non-null, or clears the
     * class name if <code>name==null</code>.
     *
     * @layer<codegen>
     */
    public  ClassBuilder setClassName( String name ) {
        className = name ;
        return this ;
    }

    /**
     * Sets the super class name, if <code>name</code> is non-null, or
     * clears the super class if <code>name==null</code>.  Note: assigning
     * a super class is incompatible with the {@link Modifier#REFINES}
     * modifier.
     *
     * @layer<codegen>
     */
    public  ClassBuilder setSuperName( String name ) {

        if ( modifiers.contains( Modifier.REFINES ) && name != null )
            throw new IllegalArgumentException( "invalid w/ REFINES" ) ;

        superName = name ;
        return this ;
    }

    /**
     * Returns a {@link String} containing the generated code for the
     * currently defined class, complete with line separators.
     *
     * @layer<codegen>
     */
    public String toString() {

        CodeBuffer buffer = new CodeBuffer() ;

        if ( layerName != null && layerName.length() > 0 ) {
            buffer.append( Keyword.LAYER ) ;
            buffer.append( layerName ) ;
            buffer.append( ';' ) ;
            buffer.endLine().endLine() ;
        }

	if (imports.size () > 0) {
	    for (Iterator p = imports.iterator () ; p.hasNext () ; ) {
		buffer.append (Keyword.IMPORT) ;
		buffer.append ((String) p.next ()) ;
		buffer.append( ';' ) ;
		buffer.endLine() ;
	    }
            buffer.endLine() ;
	}

        for ( Iterator p = modifiers.iterator() ; p.hasNext() ; )
            buffer.append( ( Modifier ) p.next() ) ;

        buffer.append( Keyword.CLASS ) ;
        buffer.append( className ) ;

        if ( superName != null ) {
            Object[] phrase = new Object[] {Keyword.EXTENDS, superName} ;
            buffer.append( Arrays.asList( phrase ) ) ;
        }

        if ( interfaces.size() > 0 ) {

            List phrase = new ArrayList( interfaces ) ;
            Collections.sort( phrase ) ;

            ListIterator p = phrase.listIterator( phrase.size() - 1 ) ;
            while ( p.hasPrevious() )
                p.set( ( ( String ) p.previous() ) + ',' ) ;

            phrase.add( 0, Keyword.IMPLEMENTS ) ;
            buffer.append( phrase ) ;
        }

        buffer.append( "{" ).endLine() ;
        buffer.indent() ;

        // Insert any lines built by direct calls to "append", etc.:
        //
        String header = super.toString().trim() ;
        if ( header != null && header.length() > 0 ) {
            buffer.endLine() ;
            buffer.appendLines( header ) ;
            buffer.endLine() ;
        }

        for ( Iterator p = methods.values().iterator() ; p.hasNext() ; ) {
            buffer.endLine() ;
            buffer.appendLines( p.next().toString() ) ;
            buffer.endLine() ;
        }

        buffer.outdent() ;
        buffer.endLine().append( "}" ) ;

        return buffer.toString() ;
    }

    //-------------------------------------------------------------------//
    // Private state information:
    //-------------------------------------------------------------------//

    private String layerName = null ;
    private String className = null ;
    private String superName = null ;

    final private Set imports = new TreeSet() ;
    final private Set interfaces = new TreeSet() ;
    final private Map methods = new TreeMap() ;
    final private Set modifiers = new TreeSet() ;

}
