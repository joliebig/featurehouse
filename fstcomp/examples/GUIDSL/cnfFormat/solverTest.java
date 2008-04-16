// SAT Solver Test (SST)
//
import java.io.*;
import java.util.*;

class solverTest {
    static final String input2SATSolver = "_debug.cnf";

    // this method does double-duty.
    // if 2 command-line arguments are given, the first file input
    // is assumed to be a #true-#end test
    // if only 1 file input is given, it is assumed to be a .cnf file

    public static void main( String args[] ) {
        if ( args.length > 1 ) {
            try {
                modelDebug( args[0] , true );
            }
            catch ( Exception e ) {
                outln( "Exception in processing " + args[0] + " " +
                                                                                             e.getMessage() );
                outln( "Processing of " + args[0] + " aborted" );
            }
            return;
        }
        original( args );
    }

    static void modelDebug( String filename, boolean saveInFile ) 
     throws dparseException, IOException {
        boolean result;
		  variable var = null;

        // Step 0 -- for debugging cnf files
                  // variable.dumpVariablesInOrder();

        // Step 1: create model cnf file string
        //
        cnfModel model = cnfModel.init();
        boolean testSucceeded = true;

        outln( "Beginning test : " + filename );
        outln();

        dparser d = new dparser( filename );
        while ( true ) {
            SATtest t = d.getNextTest();
            if ( t == null )
                break;

            if (t.isComplete()) {
				   // temporarily remove UserSelections
					ArrayList orig = grammar.UserSelections;
					grammar.UserSelections = new ArrayList();

               // now perform the complete test
					// convert strings into variable references
					grammar.UserSelections.clear();
					Vector v = t.getSelections();
					for (int i=0; i<v.size(); i++) {
					   var = (variable) (var.Vtable.get(v.get(i)));
						grammar.UserSelections.add(var);
					}
					grammar.propagate();
					testSucceeded = reportResult( t.getName(), 
					                cnfClause.complete(false) == t.isSat, testSucceeded);

					// replace original UserSelections
					grammar.UserSelections = orig;
					continue;
				}

            // create the file to be tested, invoke the solver
            // and output the result
            
            SATSolver s = new SATSolver();
            if ( saveInFile ) {
                createOutputFile( model, t );
                result = s.solve( input2SATSolver );
            }
            else { // use in-memory file
                createOutputBuffer( model, t );
                result = s.solve( new LineNumberReader( new StringReader( cnfFileString ) ) );
            }

            testSucceeded = reportResult( t.getName(),
				                result == t.isSat, testSucceeded );
        }
        outln();
        out( "Summary of " + filename + " test : " );
        if ( testSucceeded )
            outln( " ALL SUCCEEDED" );
        else
            outln( " SOME FAILED" );
    }

	 static boolean reportResult( String testname, boolean resultOfTest, boolean testSucceeded ) {
	    if (resultOfTest) {
          outln( "succeeded ... " + testname );
			 return testSucceeded;
		 }
       else {
          outln( "FAILED    ... " + testname );
          return false;
		 }
    }

    static void outln( String x ) {
        if ( ModelDebuggerGui.itsme == null )
            System.out.println( x );
        else
            ModelDebuggerGui.itsme.println( x );
    }

    static void outln() {
        outln( "" );
    }

    static void out( String x ) {
        if ( ModelDebuggerGui.itsme == null )
            System.out.println( x );
        else
            ModelDebuggerGui.itsme.print( x );
    }

    static void createOutputFile( cnfModel model, SATtest t ) 
     throws IOException, dparseException {
        PrintWriter pw = new PrintWriter( new FileWriter( input2SATSolver, false ) );
        createFile(pw, model, t);
        pw.close();
    }

    static String cnfFileString = "";

    static void createOutputBuffer( cnfModel model, SATtest t )
     throws IOException, dparseException {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter( sw );
        createFile(pw, model, t);
        pw.close();
        cnfFileString = sw.toString();
        // System.out.println(cnfFileString);
    }

    static void createFile( PrintWriter pw, cnfModel model, SATtest t ) 
     throws IOException, dparseException {
        cnfout out = new cnfout();
        t.toCnfFormat( out );
        int nclause = model.nclause + out.getCnt(); 
        pw.println( "p cnf " + model.nvars + " " + nclause + " ");
        variable.dumpVariablesInOrder(pw);
        pw.print( model.model );
        pw.println( out.toString() );
    }
}
