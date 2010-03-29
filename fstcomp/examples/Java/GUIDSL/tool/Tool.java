import Jakarta.util.*;
import java.io.*;
import java.util.*;

// This class provides a programming-interface to guidsl,
// and avoids the display of the GUI itself.  It is to be used
// for invoking the functionality of a guidsl tool.

// basic approach:
// Tool tool = new Tool( "modelFileName" );
// boolean result = true/false -- result of the SAT test
// SATtest t = new SATtest( "name of test" , result );
// t.add( "featureName or -FeatureName); // loop over this
// boolean output = true (for debugging)
// if (tool.modelDebug(t, output)) ... else ...;

public class Tool {

    cnfModel model;

    public Tool ( String modelFileName ) {

        // Step 1: open the guidsl model file 

        FileInputStream  inputFile = null;
        try {
            inputFile = new FileInputStream( modelFileName );
        }
        catch ( Exception e ) {
            Util.fatalError( "File " + modelFileName + " not found:"
               + e.getMessage() );
        }

        // Step 2: create a parser and parse input files
        //         inputRoot is root of parse tree of input file

        Parser myParser = Parser.getInstance( inputFile );
        Model    inputRoot = null;
        try {
            inputRoot = ( Model ) myParser.parseAll() ;
        }
        catch ( Exception e ) {
            Util.fatalError( "Parsing Exception Thrown in "
                             + modelFileName + ": " + e.getMessage() );
        }

        // Step 3: transform parse tree here into the internal format

        try {
           Main.process( inputRoot );
        }
		  catch( SemanticException e ) {
            int errorCnt = Util.errorCount();
		      Util.fatalError( errorCnt + " error(s) found");
		  }

		  // Step 4: initialize any additional state variables

        model = cnfModel.init();

    } // end constructor

   public boolean modelDebug( SATtest t, boolean saveInFile )  {
	   boolean result = false;

      SATSolver s = new SATSolver();
		try {
         if ( saveInFile ) {
            solverTest.createOutputFile( model, t );
            result = s.solve( solverTest.input2SATSolver );
         }
         else { // use in-memory file
            solverTest.createOutputBuffer( model, t );
            result = s.solve( new LineNumberReader( 
			                     new StringReader( solverTest.cnfFileString ) ) );
         }
		}
		catch (Exception e) {
		   Util.fatalError( "failed in debugging model " + e.getMessage() );
		}
		return result == t.isSat;
   }
}
