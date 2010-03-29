// helps debug dparser -- reads in name of a debug file
// from command line, and iterates until end of debug file
// is reached.

public class dparseTest {
    public static void main( String args[] ) {
        try {
            dparser d = new dparser( args[0] );
            while ( true ) {
                SATtest t = d.getNextTest();
                if ( t == null )
                    break;
                t.print();
            }
        }
        catch ( dparseException d ) {
            System.err.println( d.getMessage() );
            System.err.println( "Processing of " + args[0] + " aborted" );
            System.exit( 1 );
        }
        System.err.println( "Processing of " + args[0] + " completed" );
    }
}
