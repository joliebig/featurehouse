import Jakarta.util.*;
import java.io.*;
import java.util.*;

class Main {
    static boolean debug = false;
    static boolean printFile = false;
    static boolean dumpPtable = false;
    static boolean dumpVtable = false;
    public static boolean modelMode = false;
    static String  inputFileName = "";
    static final String defaultModelFile = "model.m";

    static void marquee() {
        System.out.println( "Usage:  guidsl <options> <file>.m" );
        System.out.println( "        <file>.m is a feature model" );
        System.out.println( "        options -d debug" );
        System.out.println( "                -p print input file" );
        System.out.println( "                -m model mode uses 'model.m'" );
        Main.marqueeAdditions();
        System.exit( 1 );
    }

    // the following methods are extension points for the Main class

    static void marqueeAdditions() {
    // layers extend this method with their option descriptions
    }

    static boolean processOptions( char o ) {
        // layers extend this method with option processing
             // default returns false, but this should be overridden
        return false;
    }

    static void debugActions() {
    // layers extend this method with debug reporting
    }

    public static void process( Model root ) throws SemanticException {
    // layers extend this method for AST processing
    }

    public static void process2() {
    // layers extend this method for gspec object procssing
    }

    public static void main( String[] args ) {
        int                 argc = args.length;
        int                 non_switch_args;

        // Step 1: a general routine to pick off command line options
        //         options are removed from command line and
        //         args array is adjusted accordingly.
        //         right now, there are no command-line options
        //         but this code is here for future expansion

        non_switch_args = 0;
        for ( int i=0; i < argc; i++ ) {
            if ( args[i].charAt( 0 ) == '-' ) {

                // switches of form -xxxxx (where xxx is a sequence of 1
                // or more characters

                for ( int j=1; j < args[i].length(); j++ ) {
                    char o = args[i].charAt( j );
                    if ( Main.processOptions( o ) )
                        continue;
                    if ( o == 'd' ) {
                        debug = true;
                        continue;
                    }
                    if ( o == 'p' ) {
                        printFile = true;
                        continue;
                    }
                    if ( o == 'm' ) {
                       modelMode = true;
                       continue;
                    }
                    System.err.println( "Unrecognizable option " + o );
                    Main.marquee();

                // if (args[i].charAt(j) == 'x' {
                //        ... do this for option 'x'
                // }
                }
            }
            else {
                // non-switch arg

                args[non_switch_args] = args[i];
                non_switch_args++;
            }
        }

        // assume "model.m" as the default name of a file.
        // we use this default if non_switch_args == 0 and
        // there is no such "model.m" file in the current directory

        if ( non_switch_args == 0 ) {
            if (modelMode)
               inputFileName = defaultModelFile;
            else
               Main.marquee();
        }
        else inputFileName = args[0];

        // Step 2: open file

        FileInputStream  inputFile = null;
        try {
            inputFile = new FileInputStream( inputFileName );
        }
        catch ( Exception e ) {
            System.err.println( "File " + inputFileName + " not found:"
               + e.getMessage() );
            Main.marquee();
        }

        // Step 3: create a parser and parse input files
           //         inputRoot is root of parse tree of input file

        Parser myParser = Parser.getInstance( inputFile );
        Model    inputRoot = null;
        try {
            inputRoot = ( Model ) myParser.parseAll() ;
        }
        catch ( Exception e ) {
            System.out.println( "Parsing Exception Thrown in "

                                + inputFileName + ": " + e.getMessage() );
            System.exit( 1 );
        }

        // Step 4: Initialize output stream to standard out
         //         Standard initialization stuff that should be
         //         platform independent.

        PrintWriter pw            = null;

        AstProperties props = new AstProperties();
        String lineSeparator      =
                System.getProperties().getProperty( "line.separator" );

        if ( lineSeparator.compareTo( "\n" ) != 0 )
            pw = new PrintWriter( new FixDosOutputStream( System.out ) );
        else
            pw = new PrintWriter( System.out );

        props.setProperty( "output", pw );

        // Step 5: transform parse tree here

        try {
           Main.process( inputRoot );
        }
          catch( SemanticException e ) {
            int errorCnt = Util.errorCount();
            System.err.println( Util.errorCount() + " error(s) found");
            System.err.println( "Processing terminated" );
                System.exit(1);
          }

        Main.process2();

        if ( printFile ) {
            inputRoot.print();
            System.out.println();
        }
        if ( debug ) {
            Main.debugActions();
        }
    } //end main()
}
