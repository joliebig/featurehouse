import java.io.*;
import java.util.*;

// Verify compares the cnf encoding with the comments in _debug.cnf files
// produced by guidsl.  it is useful for debugging and to make sure
// that the input to sat4j is the same as that being used by guidsl

public class Verify {

    static LineNumberReader l;
    static String predicate = null;
    static int lineno;
    static String line;
    static String[] varTable = new String[1000];

    public static void main( String[] args ) {
        StringTokenizer st;
        boolean usedAnd = false;
        int cctr = 0;
        int number;
        boolean testPredicate = false;
        String saved = "";
        int lineno = -1;

        System.out.print("verifying _debug.cnf");
        try {
            l = new LineNumberReader( new FileReader( "_debug.cnf" ) );

            int ccntr = 0;
            while ( true ) {
                lineno = l.getLineNumber();
                String line = l.readLine();

		// Step 1: quit at end of file
                if ( line == null ) {
                    l.close();
                    break;
                }

		// Step 2: ignore blank lines
		if ( line.equals(""))
			continue;

                // Step 3: initialize varTable with line 
		//         beginning with p cnf # #
                if ( line.charAt( 0 ) == 'p' ) {
                    st = new StringTokenizer( line );
                    String p = st.nextToken();
                    String cnf = st.nextToken();
                    number = Integer.parseInt( st.nextToken() );
                    varTable = new String[number+1]; // never access varTable[0]
                    continue;
                }

		// Step 4: lines beginning with 'cc' define a variable and
		//         its index
                if ( line.startsWith( "c c" ) ) {
                    st = new StringTokenizer( line.substring( 3 ) );
                    number = Integer.parseInt( st.nextToken() );
                    String varname = st.nextToken();
                    varTable[number] = varname;
                    continue;
                }

		// Step 5: lines beginning with 'cx' define a cnf predicate
		//         string
		if ( line.startsWith("c x") ) {
                       if ( predicate != null ) {
                            if ( usedAnd )
                                predicate = "(" + predicate + ")";
                            if ( !saved.equals( predicate ) ) {
                                System.out.println();
                                System.out.println( "on line " + lineno );
                                System.out.println( "t:>" + saved );
                                System.out.println( "c:>" + predicate );
                                System.exit( 1 );
                            }
		        }
                        saved = line.substring( 4 );
			predicate = null;
			usedAnd = false;
		}


		// Step 7: ignore lines beginning with 'c' otherwise
                if ( line.charAt( 0 ) == 'c' )
		   continue;

                // Step 8: assume this is a line in cnf format
		//         we parse the line and add it to predicate

                st = new StringTokenizer( line );
                String clause = null;
                boolean usedOr = false;
                while ( st.hasMoreTokens() ) {
                    number = Integer.parseInt( st.nextToken() );
                    if ( number == 0 )
                        break;
                    if ( clause == null )
                        clause = varRef( number );
                    else {
                        clause += " or " + varRef( number );
                        usedOr = true;
                    }
                }
                if ( usedOr )
                    clause = "("+clause+")";
                if ( predicate == null )
                    predicate = clause;
                else {
                    predicate = predicate + " and " + clause;
                    usedAnd = true;
                }
            }
        }
        catch ( Exception e ) {
            System.err.println( "on lineno " + lineno + ":" + line );
            System.err.println( e.getMessage() );
        }
        System.out.println("... succeeded!");
    }

    static String varRef( int i ) {
        if ( i < 0 )
            return "not " + varTable[-i];
        else
            return varTable[i];
    }
}
