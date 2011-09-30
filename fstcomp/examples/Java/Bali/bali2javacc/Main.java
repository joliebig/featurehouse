

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

//-----------------------------------------------------------------------//
// Main class:
//-----------------------------------------------------------------------//

/**
 * Provides a <code>driver</code> to translate a Bali source file into a
 * Javacc source file.
 *
 * @layer<bali2javacc>
 */
    
public class Main {

    final private static String PROPERTIES = "bale2javacc.properties" ;

    /**
     * Handles argument parsing, source parsing, parse-tree collection,
     * and code generation.
     *
     * @return {@link $TEqn.Collector} with collected data from parse tree.
     *
     * @layer<bali2javacc>
     */
    public Object driver( String[] args ) throws Throwable {

        setVersion( "v2002.09.04" ) ;

        Files.setProgram( "bali2javacc" ) ;
        Files.setVersion( getVersion() ) ;

        loadProperties( PROPERTIES ) ;

        try {
            parseArguments( Arrays.asList( args ) ) ;
        }
        catch ( Exception exception ) {
            usage( exception.getMessage() ) ;
            throw exception ;
        }

        Main.DEBUG.info( "-output " + String.valueOf( argOutputFile ) ) ;
        Main.DEBUG.info( "-package " + String.valueOf( argPackage ) ) ;
        Main.DEBUG.info( "source " + String.valueOf( argSourceFile ) ) ;

        Collector collector = collectSource( argSourceFile ) ;
        collector.setPackage( argPackage ) ;
        generateObject( collector ) ;

        Main.DEBUG.exiting( "bali2javacc.Main", "driver", "collector" ) ;
        return collector ;
    }

    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    /**
     * Given an {@link Object}, returns a {@link String} containing the
     * normalized {@link Class} name for the object.
     *
     * @layer<bali2javacc>
     */
    private static String className( Object object ) {
        return
                ( object != null )
		? object.getClass().getName()
		: "null" ;
    }

    /**
     * Processes an input {@link File} (with Bali source code) into a parse
     * tree, then running a {@link $TEqn.Collector} over the tree to gather
     * all necessary data for later code generation.
     *
     * @return {@link $TEqn.Collector} with collected data from parse tree.
     *
     * @layer<bali2javacc>
     */
    private  Collector collectSource( File inpFile )
    throws IOException, ParseException {
        Main.DEBUG.entering( "bali2javacc.Main", "collectSource", inpFile ) ;

        Reader reader = new BufferedReader( new InputStreamReader( new FileInputStream( inpFile ),
                    "ISO-8859-1" ) ) ;

        Parser parser =  Parser.getInstance( reader ) ;
        BaliParse tree = (BaliParse) parser.parseAll () ;
        reader.close() ;

        Collector collector = new  Collector() ;
        collector.dispatch( tree ) ;

        Main.DEBUG.exiting( "bali2javacc.Main", "collectSource" ) ;
        return collector ;
    }

    /**
     * Generates the object code {@link File} objects (Jak class files for
     * parse tree nodes) from a {@link $TEqn.Collector} argument.
     *
     * @layer<bali2javacc>
     */
    public void generateObject( Collector collector )
    throws IOException {
        Main.DEBUG.entering( "bali2javacc.Main", "generateObject" ) ;
        Files.toFile( collector.toString(), argOutputFile ) ;
        Main.DEBUG.exiting( "bali2javacc.Main", "generateObject" ) ;
    }

    public void loadProperties( String source ) {
        Main.DEBUG.entering( "bali2javacc.Main", "loadProperties", source ) ;

        // Allow users to rename resource via system properties:
        //
        String resource = System.getProperty( source, source ) ;

        // Load properties from properties resource:
        //
        Properties properties = new Properties() ;
        try {
            properties.load( ClassLoader.getSystemResourceAsStream( resource ) ) ;
            Main.DEBUG.info( "properties loaded from resource " + resource ) ;
        }
        catch ( NullPointerException thrown ) {
            return ; // Resource not found is ok.
        }
        catch ( IOException thrown ) {
            IllegalStateException exception = 
                            new IllegalStateException( "resource error " + resource ) ;
            exception.initCause( thrown ) ;
            throw exception ;
        }

        // Create a new system Properties object containing the current
        // system properties, but using the application Properties as
        // defaults.  This way, properties defined on the command line
        // (which show up as system properties) override application
        // properties defined via relatively static sources such as
        // resources and files.
        //
        Properties newSystem = new Properties( properties ) ;
        newSystem.putAll( System.getProperties() ) ;
        System.setProperties( newSystem ) ;

        Main.DEBUG.exiting( "bali2javacc.Main", "loadProperties" ) ;
    }

    /**
     * Processes a {@link List} of {@link String} arguments that specify
     * the input files and output file.
     *
     * @see #usage()
     *
     * @layer<bali2javacc>
     */
    private void parseArguments( List args ) throws IOException {

        // Set default values:
        //
        argDebug = Level.OFF ; // How much debugging output.
        argOutputFile = null ; // JavaCC output file.
        argPackage = null ; // Generated package name.
        argSourceFile = null ; // Bali source file.

        for ( ListIterator p = args.listIterator() ; p.hasNext() ; ) {

            String arg = ( String ) p.next() ;
            if ( arg.equals( "-debug" ) ) {
                argDebug = Level.INFO ;
                if ( p.hasNext() ) {
                    String peek = ( String ) p.next() ;
                    if ( peek.charAt( 0 ) != '-' )
                        argDebug = Level.parse( peek.toUpperCase() ) ;
                    else
                        p.previous() ;
                }
                continue ;
            }

            if ( arg.equals( "-output" ) && p.hasNext() )
                argOutputFile = parseOutputFile( ( String ) p.next() ) ;
            else
                if ( arg.equals( "-o" ) && p.hasNext() )
                    argOutputFile = parseOutputFile( ( String ) p.next() ) ;
                else
                    if ( arg.equals( "-package" ) && p.hasNext() )
                        argPackage = parsePackage( ( String ) p.next() ) ;
                    else
                        if ( arg.equals( "-p" ) && p.hasNext() )
                            argPackage = parsePackage( ( String ) p.next() ) ;
                        else
                            if ( arg.charAt( 0 ) == '-' )
                                throw new IllegalArgumentException( "invalid: " + arg ) ;
                            else
                                argSourceFile = parseSourceFile( arg ) ;
        }

        Main.DEBUG.setLevel( argDebug ) ;

        if ( argSourceFile == null ) {
            String message = "no Bali source file specified" ;
            throw new IllegalArgumentException( message ) ;
        }

        if ( argOutputFile == null ) {
            String name = argSourceFile.getName() ;
            int dot = name.lastIndexOf( '.' ) ;
            name = ( ( dot < 0 ) ? name : name.substring( 0, dot ) ) + ".jj" ;
            argOutputFile = new File( argSourceFile.getParentFile(),name ) ;
            argOutputFile = validateOutputFile( argOutputFile ) ;
        }
    }

    private File parseOutputFile( String fileName ) {

        if ( argOutputFile != null )
            throw new IllegalArgumentException( "option \"-output\" appears more than once" ) ;

        return validateOutputFile( new File( fileName ) ) ;
    }

    private String parsePackage( String packageName ) {

        if ( argPackage != null )
            throw new IllegalArgumentException( "option \"-package\" appears more than once" ) ;

        if ( ! packageName.matches( "^[\\p{Alpha}_$][\\p{Alnum}_$]*$" ) )
            throw new IllegalArgumentException( "option \"-package\" doesn't specify an identifier" ) ;

        return packageName ;
    }

    private File parseSourceFile( String fileName ) {

        if ( argSourceFile != null )
            throw new IllegalArgumentException( "more than one Bali source file specified" ) ;

        File file = new File( fileName ) ;

        if ( ! file.exists() )
            throw new IllegalArgumentException( "file doesn't exist: "
                            + fileName ) ;

        if ( ! file.canRead() )
            throw new IllegalArgumentException( "file can't be read: "
                            + fileName ) ;

        return file ;
    }

    private void usage( String message ) {

        String program = className( this ) ;

        if ( message != null )
            System.err.println( program + ": " + message ) ;

        System.err.println( "Usage: java "
                + program
                + " [-output <JavaCC-output-file>]"
                + " [-package <Java-package-name>]"
                + " <Bali-source-file>" ) ;
    }

    private File validateOutputFile( final File outputFile ) {

        if ( ! outputFile.exists() )
            return outputFile ;

        if ( ! outputFile.canWrite() )
            throw new IllegalArgumentException( "file \""
                            + outputFile
                            + "\" cannot be written" ) ;

        if ( outputFile.isDirectory() )
            throw new IllegalArgumentException( "file \""
                            + outputFile
                            + "\" is not a normal file" ) ;

        return outputFile ;
    }

    private Level argDebug = Level.OFF ; // How many debug messages.
    private String argPackage = null ; // Generated package name.
    private File argOutputFile = null ; // Output file.
    private File argSourceFile = null ; // Bali source file.

}
