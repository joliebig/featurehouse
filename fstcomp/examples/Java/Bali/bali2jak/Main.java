layer bali2jak;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;

//-----------------------------------------------------------------------//
// Main class:
//-----------------------------------------------------------------------//

/**
 * Provides a <code>driver</code> to translate a Bali source file into a
 * set of Jak source files.
 *
 * @layer<bali2jak>
 */
    
public refines class Main {

    final private static String PROPERTIES = "bali2jak.properties" ;

    /**
     * Handles argument parsing, source parsing, parse-tree collection,
     * and code generation.
     *
     * @return {@link $TEqn.Collector} with collected data from parse tree.
     *
     * @layer<bali2jak>
     */
    public Object driver( String[] args ) throws Throwable {

        setVersion( "v2002.09.03" ) ;

        Files.setProgram( "bali2jak" ) ;
        Files.setVersion( getVersion() ) ;

        loadProperties( PROPERTIES ) ;

        try {
            parseArguments( Arrays.asList( args ) ) ;
        }
        catch ( Exception exception ) {
            usage( exception.getMessage() ) ;
            throw exception ;
        }

        Main.DEBUG.info( "-layer " + String.valueOf( argLayer ) ) ;
        Main.DEBUG.info( "-directory " + String.valueOf( argDirectory ) ) ;
        Main.DEBUG.info( "source " + String.valueOf( argSourceFile ) ) ;

        Collector collector = collectSource( argSourceFile ) ;
        generateObject( collector ) ;

        Main.DEBUG.exiting( "bali2jak.Main", "driver", "collector" ) ;
        return collector ;
    }

    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    /**
     * Given an {@link Object}, returns a {@link String} containing the
     * normalized {@link Class} name for the object.
     *
     * @layer<bali2jak>
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
     * @layer<bali2jak>
     */
    private  Collector collectSource( File inpFile )
    throws IOException, ParseException {
        Main.DEBUG.entering( "bali2jak.Main", "collectSource", inpFile ) ;

        Reader reader = new BufferedReader( new InputStreamReader( new FileInputStream( inpFile ),
                    "ISO-8859-1" ) ) ;

        Parser parser =  Parser.getInstance( reader ) ;
        BaliParse tree = ( BaliParse ) parser.parseAll () ;
        reader.close() ;

        Collector collector = new  Collector() ;
        collector.dispatch( tree ) ;

        Main.DEBUG.exiting( "bali2jak.Main", "collectSource", collector ) ;
        return collector ;
    }

    /**
     * Generates the object code {@link File} objects (Jak class files for
     * parse tree nodes) from a {@link $TEqn.Collector} argument.
     *
     * @layer<bali2jak>
     */
    public void generateObject( Collector collector )
    throws IOException {
        Main.DEBUG.entering( "bali2jak.Main", "generateObject" ) ;

        collector.setLayer( argLayer ) ;

        // Generate syntax tree node classes for Bali grammar rules:
        //
        collector.generateNonterminals( argDirectory ) ;

        Main.DEBUG.exiting( "bali2jak.Main", "generateObject" ) ;
    }

    public void loadProperties( String source ) {
        Main.DEBUG.entering( "bali2jak.Main", "loadProperties", source ) ;

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

        Main.DEBUG.exiting( "bali2jak.Main", "loadProperties" ) ;
    }

    /**
     * Processes a {@link List} of {@link String} arguments that specify
     * the input files and output file.
     *
     * @see #usage()
     *
     * @layer<bali2jak>
     */
    private void parseArguments( List args ) throws IOException {

        // Set default values:
        //
        argLayer = null ; // Generated layer name.
        argDebug = Level.OFF ; // How much debugging output.
        argDirectory = null ; // Output directory.
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

            if ( arg.equals( "-layer" ) && p.hasNext() )
                argLayer = parseLayer( ( String ) p.next() ) ;
            else
                if ( arg.equals( "-a" ) && p.hasNext() )
                    argLayer = parseLayer( ( String ) p.next() ) ;
                else
                    if ( arg.equals( "-directory" ) && p.hasNext() )
                        argDirectory = parseDirectory( ( String ) p.next() ) ;
                    else
                        if ( arg.equals( "-d" ) && p.hasNext() )
                            argDirectory = parseDirectory( ( String ) p.next() ) ;
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

        if ( argDirectory == null )
            argDirectory = parseDirectory( "." ) ;

        if ( argLayer == null ) {

            File directory ;
            try {
                directory = argDirectory.getCanonicalFile() ;
            }
            catch ( IOException exception ) {
                directory = argDirectory.getAbsoluteFile() ;
            }

            argLayer = parseLayer( directory.getName() ) ;
        }

        return ;
    }

    private String parseLayer( String layerName ) {

        if ( argLayer != null )
            throw new IllegalArgumentException( "option \"-layer\" appears more than once" ) ;

        if ( ! layerName.matches( "^[\\p{Alpha}_$][\\p{Alnum}_$]*$" ) )
            throw new IllegalArgumentException( "option \"-layer\" doesn't specify an identifier" ) ;

        return layerName ;
    }

    private File parseDirectory( String directoryName ) {

        if ( argDirectory != null )
            throw new IllegalArgumentException( "option \"-directory\" appears more than once" ) ;

        File directory = new File( directoryName ) ;

        if ( ! directory.exists() )
            directory.mkdirs() ;

        if ( ! directory.isDirectory() )
            throw new IllegalArgumentException( "file \""
                            + directory
                            + "\" is not a directory" ) ;

        return directory ;
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
                + " [-layer <layer-name>]"
                + " [-directory <output-directory>]"
                + " <Bali-source-file>" ) ;
    }

    private Level argDebug = Level.OFF ; // How many debug messages.
    private String argLayer = null ; // Generated layer name.
    private File argDirectory = null ; // Output directory.
    private File argSourceFile = null ; // Bali source file.

}
