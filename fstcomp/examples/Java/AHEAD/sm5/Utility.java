

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

class Utility  {

    // this method decides whether to translate the parent state machine file
    // (and hence regenerate its .ser file.  It returns true if a retranslation
    // (i.e., recursive invocation of parse) is needed

    public static boolean retranslate( String serfilename, String jakfilename ) {

        // Step 1: get a handle to both files

        File ser = null;
        File jak = null;
        try {
            ser = new File( serfilename );
            jak = new File( jakfilename );
        }
        catch ( Exception e ) {
            AstNode.fatalError( e.getMessage() );
        }

        // Step 2: check to see if both exist

        boolean serExists = false;
        boolean jakExists = false;
        try {
            serExists = ser.exists();
            jakExists = jak.exists();
        }
        catch ( Exception e ) {
            AstNode.fatalError( e.getMessage() );
        }

        // Step 3: get the modification times of both files

        long serTime = 0;
        long jakTime = 0;
        try {
            if ( serExists )
                serTime = ser.lastModified();
            if ( jakExists )
                jakTime = jak.lastModified();
        }
        catch ( Exception e ) {
            AstNode.fatalError( e.getMessage() );
        }

        // Step 4: we must retranslate the .jak file (and thus regenerate
        //         the .ser file if (the .ser file does not exist) OR
        //         the jakfile was modified after the serfile

        boolean retrans = !serExists || jakTime > serTime;

        // Step 5: if the .jak file doesn't exist AND we have to retranslate, 
        //         we have a fundamental problem -- we can't translate 
        //         a non-existant file to produce a .ser file.
        //         Note: it is possible that a .ser file exists but no .jak
        //         file -- this happens when a single .jak file defines multiple
        //         state machines, listed in ancestor order.  In this way,
        //         parent state machines are translated first, and thus their
        //         .ser files will exist even though there is no .jak file, per se,
        //         that uniquely identifies parent state machines

        if ( retrans && !jakExists ) {
            AstNode.fatalError( "parent state machine in " + jakfilename + " does not exist" );
        }

        return retrans;

    }

    static String FileName( String n, String extension ) {
        String filename = n + extension;
        if ( kernelConstants.globals().sm4vars.ser_directory != null )
            filename =  kernelConstants.globals().sm4vars.ser_directory + 
                           File.separatorChar + filename;
        return filename;
    }

    static String SerFileName( String n ) {
        return FileName( n,  sm4data.serExtension );
    }

    static String JakFileName( String n ) {
	     // there are two ways of determining the extension of a jak file.
		  // we will first use the currentFileExt, and if that fails,
		  // default to the jakExtension set on the command-line (defaulting to .jak)

        String f = FileName( n,  kernelConstants.globals().currentFileExt );
		  try {
		     File fl = new File(f);
           if (fl.exists())
			     return f;
		  }
		  catch (Exception e) { /* ignore errors */ }
        return FileName( n,  kernelConstants.jakExtension );
    }

    static String SourceName() { // for reporting errors
        return "In State Machine " +  kernelConstants.globals().sm4vars.Sm.name + ": ";
    }

    // this method either reads a previously built .ser file, or 
    // invokes the parser to retranslate the .jak file

    public static  sdInfo LocateSuperSmFile() {

        // Step 1: get the file names for the .jak and .ser files

        String jakFile = JakFileName( kernelConstants.globals().sm4vars.Sm.superSm_name );
        String serFile = SerFileName( kernelConstants.globals().sm4vars.Sm.superSm_name );

        // Step 2: retranslate if necessary

        if ( retranslate( serFile,jakFile ) ) {
            Main.instance.processing( "jak2java", jakFile );
        }

        // Step 3: now read the ser file
        return ( sdInfo ) readObjectFromFile( serFile );
    }

    static void writeObjectToFile( Object o, String filename ) {
        ObjectOutputStream os = null;

        try {
            os = new ObjectOutputStream( new FileOutputStream( filename ) );
        }
        catch( Exception e ) {
            AstNode.fatalError( "writeObjectToFile(.., " + 
                            filename + e.getMessage() );
        }
   
        try {
            os.writeObject( o );
            os.close();
        }
        catch( Exception e ) {
            AstNode.fatalError( e.getMessage() );
        }

        // write is successful.  Now write object to $TEqn.kernelConstants.globals().sm4vars.serCache
 
        kernelConstants.globals().sm4vars.serCache.put( filename, o );
    }

    static Object readObjectFromFile( String filename ) {
        ObjectInputStream is = null;

        // first, see if object exists in the $TEqn.kernelConstants.globals().sm4vars.serCache.  If so,
        // return it, else read from file.

        Object o =  kernelConstants.globals().sm4vars.serCache.get( filename );
        if ( o != null )
            return o;

        try {
            is = new ObjectInputStream( new FileInputStream( filename ) );
            o = is.readObject();
            is.close();
        }
        catch ( Exception e ) {
            AstNode.fatalError( e.getMessage() );
        }

        // finally, put the object in the cache
        kernelConstants.globals().sm4vars.serCache.put( filename, o );
        return o;
    }
}
