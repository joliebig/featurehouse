import java.util.*;
import Jakarta.util.*;
import java.io.*;

class TraverseSet {
   static boolean debug = false;  // set to true for debugging

   static void walk( String dir ) {
	if (!dir.equals(".")) {
           String dirname = dir.substring( dir.lastIndexOf("/") + 1 );
           System.out.println("layer " + dirname );
        }

        File directory = new File( dir );
        String[] fileList = directory.list();
       
        if ( fileList == null ) {
            Util.fatalError( "'" + dir + "' is not a directory or " +
                             "it does not exist" );
        }

        for( int i = 0; i < fileList.length; i++ ) {
            if ( fileList[i].endsWith( ".jak" ) ) {
                try {
                    String fname = dir + "/" + fileList[i];
                    String[] args = new String[1];
                    //Main instance = new Main() ;
                    args[0] = fname;
                    if ( debug )
                        System.out.println( ">>> mmatrix.Main " + args[0] );
		    System.out.println( "file " + fileList[i]);
                    //instance.main( args ) ;
		    Main.main(args);
                }
                catch ( Throwable thrown ) {
                    thrown.printStackTrace() ;
                    System.exit( 1 ) ;
                }
            }
            else {
                if (debug)
                   System.out.println(">>> testing " + fileList[i]);

		// ignore CVS directories
                if ( fileList[i].endsWith( "CVS" ))
                    continue;

                File f = new File( dir + "/" + fileList[i] );
                if( f.exists() && f.isDirectory() )  {
                   if ( debug )
                    System.out.println( ">>> " + dir + "/" + fileList[i] );
                   walk( dir + "/" + fileList[i] );
		} else {
                   if ( debug )
                    System.out.println( ">>> skipping" + 
				         dir + "/" + fileList[i] );
		}
            }
        }
    }
}
