layer bali2layer ;

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
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.regex.*;
import java.util.logging.Level;

import java.io.*;
import java.util.*;
import java.lang.*;


//-----------------------------------------------------------------------//
// Main class:
//-----------------------------------------------------------------------//

/**
 * Provides a <code>driver</code> to translate a Bali source file into a set of
 * Jak source files.
 */
public refines class Main {
        
    final public static String resourcePrefix = "resource/" ;

    /**
     * Handles argument parsing, data collecting, and code generation
     */
    public Object driver( String[] args ) throws Throwable {

	Arguments argsData = null;
	try {
	    argsData = parseArguments( Arrays.asList( args ) ) ;
        }
        catch ( IllegalArgumentException exception ) {
	    System.err.println(exception.getMessage());
	    usage () ;
	    System.exit (1) ;
	}
	argsData.collectClass();
	generateObject(argsData);
	return null;
    } 
  
 
    /**
     * Generates Jak files and, optionally, a Main class.
     */
    public void generateObject(Arguments argsData) {
        genJakFile(argsData);
        genMain(argsData);
    }


    /**
     * Processes a {@link List} of {@link String} arguments that specify
     * the input file and options.
     *
     * @see #usage()
     */
    private Arguments parseArguments( List args ) throws IOException {

	Arguments argsData = new Arguments () ;
        for ( Iterator p = args.iterator() ; p.hasNext() ; ) {
                        
            String arg = ( String ) p.next();

	    if (arg.charAt (0) != '-')
		throw new IllegalArgumentException (
		    "unrecognized argument: "
		    + arg
		);
		
	    // Options without a value parameter go here:
	    //
	    if( arg.equals( "-f" ) ) {
		argsData.setOutputMain(true);
		argsData.setFileInput(true);
		continue;
	    }

	    //output Main with std input
	    if( arg.equals( "-s" ) ) {
		argsData.setOutputMain(true);
		argsData.setStdInput(true);
		continue;
	    }

	    // Options that require a single value parameter go here:
	    //
	    if (! p.hasNext ())
		throw new IllegalArgumentException (
		    "no value specified for option: "
		    + arg
		) ;

	    //get classname file 
	    if( arg.equals( "-l" ) ) {
		argsData.setClassFile( (String) p.next () );
		continue;
	    }

	    //get grammar file
	    if( arg.equals( "-b" ) ) {
		argsData.setBaliFile( (String) p.next () );
		continue;
	    }

	    //parse all methods                              
	    if( arg.equals( "-m" ) ) {
		String[] methods = ((String) p.next ()) . split (";") ;
		for( int i = 0; i < methods.length; i++ )
		    argsData.addMethod(methods[i]);
		continue;
	    }

	    //get layer name
	    if( arg.equals( "-layer" ) ) {
		argsData.setLayer( (String) p.next () );
		continue;
	    }

	    //get start name
	    if( arg.equals( "-start" ) ) {
		argsData.setStart( (String) p.next () );
		continue;
	    }
                                                
	    throw new IllegalArgumentException (
		"unrecognized option: "
		+ arg
	    ) ;
        }

	return argsData ;
    }

    protected static void usage() {
        System.err.println( "Usage: bali2layer.Main [-f] [-s]" +  
                "[-m \"<Method> [;<method>]\"]  [-l classFile]" + 
                "[-b grammar file] -start sname -layer lname");
        System.err.println( "  -f        output main class with file input" );
        System.err.println( "  -s        output main class with std input" );
        System.err.println( "  -m 	 specify methods");
        System.err.println( "  -l        file of a list of classes" );
        System.err.println( "  -b        bali grammer file" );
        System.err.println( "  sname     name of start production" );
        System.err.println( "  lname     name of layer file" );
        System.err.println( "  ClassFile file containing class names to generate" );
    }
 

    /**
     * Generates Jak files
     */    
    private static void genJakFile(Arguments args) {
 
	for(Iterator p = args.getClasses().iterator(); p.hasNext(); ) {
	    String className = (String) p.next();
	    templateClass(args, className) ;
	}  
        
    }

    /**
     * Generates a Main class
     */
    private static void genMain(Arguments args) {

        try{
            if (args.doesOutputMain()) {

		PrintWriter outfile = new PrintWriter( new BufferedWriter( 
			    new FileWriter("Main.jak"))) ;
		outfile.println("layer " + args.getLayer() + ";");
		copyResource( outfile, resourcePrefix + "JakFileHeader.txt" );
                if ( args.doesFileInput()) {
                    copyResource( outfile, resourcePrefix + "FileInputTemplateHeader.txt" );
                    outfile.println( "         "+args.getStart()+"    inputRoot = null;" );
                    outfile.println( "         try {" );
                    outfile.println( "            inputRoot = ("+args.getStart()+") myParser.parseAll() ;" );
                    copyResource( outfile, resourcePrefix + "FileInputTemplateFooter.txt" );
                }
                else {
                    copyResource( outfile, resourcePrefix + "StdInputTemplateHeader.txt" );
                    outfile.println();
                    outfile.println( "            (("+args.getStart()+") root).print();" );
                    outfile.println( "            System.out.println();" );
                    outfile.println( "            (("+args.getStart()+") root).execute();" );
                    outfile.println( "            pw.flush();" );
                    outfile.println( "" );
                    outfile.println( "            // Step 5.5: now dump the parse tree" );
                    outfile.println( "            //           this code can be removed for production systems" );
                    outfile.println( "" );
                    outfile.println( "            System.out.println(\"Dump root\");" );
                    outfile.println( "            root.PrettyDump();" );
                    outfile.println( "" );
                    outfile.println( "         } while (true);          // end Language statement loop" );
                    outfile.println( "      } //end main()" );
                    outfile.println( "   }  // end Main class" );
                }
		outfile.close() ;
            }
	}
	catch (IOException e) {
	    System.err.println( e.getMessage() );
	}
    }

    /**
     * Copies a template to a file
     */
    private static void copyResource( PrintWriter outfile, String resource ) {
	InputStream inp = null ;
        try {
            inp = Main.class.getResourceAsStream(resource) ;
            BufferedReader reader = new BufferedReader (
		new InputStreamReader(inp, "ISO-8859-1")
	    ) ;

	    String line;
	    while ((line = reader.readLine()) != null)
		outfile.println (line) ;

	    inp.close();

        } catch ( IOException e ) {
            System.err.println( e.getMessage() );
        }
    }

    /**
     * Generates Jak files
     *
     */
    private static void templateClass( Arguments args, String className) {

	ClassBuilder klass = new ClassBuilder () ;
	klass.addModifier (Modifier.REFINES) ;
	klass.setClassName (className) ;

	for (Iterator p = args.getMethods().iterator () ; p.hasNext () ; )
	    klass.addMethod ( (MethodBuilder) p.next () ) ;

	PrintWriter PWriter = null ;
	try {
	    PWriter =new PrintWriter( new BufferedWriter(  
		    new FileWriter (className + ".jak"))) ;
	    PWriter.println("layer " + args.getLayer() + ";");
  	    copyResource(PWriter, resourcePrefix + "JakFileHeader.txt" );
	    PWriter.println (klass.toString ()) ;
	    PWriter.close();
	}
	catch (IOException exception) {
	    System.err.println (exception.getMessage ()) ;
	    System.exit (1) ;
	}
    }

}
