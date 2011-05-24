

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// complete override of prior version -- so that all utility methods
// do nothing

class Utility {

    // override -- always return false as we never want to
	 // recursively parse
    public static boolean retranslate( String serfilename, 
	     String jakfilename ) { return false; }

    static String FileName( String n, String extension ) {
        return n;
    }

    static String SerFileName( String n ) {
        return n;
    }

    static String JakFileName( String n ) {
        return n;
    }

    static String SourceName() { // for reporting errors
        return "In State Machine " +  kernelConstants.globals().sm4vars.Sm.name + ": ";
    }

    // should never be invoked

    public static  sdInfo LocateSuperSmFile() {
	     AstNode.fatalError( "should not call LocalSuperSmFile()" );
        return null;
    }

    static void writeObjectToFile( Object o, String filename ) { }

    static Object readObjectFromFile( String filename ) { return null; }
}
