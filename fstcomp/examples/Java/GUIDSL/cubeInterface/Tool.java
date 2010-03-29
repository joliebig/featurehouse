//created on: Sun Oct 30 14:08:51 CST 2005

class Tool {


   public boolean modelDebug( SATtest t, boolean saveInFile, StringBuffer out )  {
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
         out.append(s.decode(true));
        }
        catch (Exception e) {
           Util.fatalError( "failed in debugging model " + e.getMessage() );
        }
        return result == t.isSat;
   }


/*
    public Tool (String modelFileName){
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
    }
    */
/*
    public Tool ( StringBuffer modelString ) {



        // Step 2: create a parser and parse input files
        //         inputRoot is root of parse tree of input file

        Parser myParser = Parser.getInstance( new StringReader(modelString.toString()) );
        Model    inputRoot = null;
        try {
            inputRoot = ( Model ) myParser.parseAll() ;
        }
        catch ( Exception e ) {
            Util.fatalError( "Parsing Exception Thrown "
                             + e.getMessage() );
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
*/
}