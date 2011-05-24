// SAT Solver Test (SST)
//

import java.io.*;


// accepts file as input.
// if satisfiable, then the solutions are printed

public class solverTest {
   public static void main( String args[] ) {
      if (args.length !=1 ) {
         System.err.println(" must supply a .cnf file command-line argument");
         System.exit(1);
      }
      SATSolver s = new SATSolver();
      boolean result = s.solve(args[0]);
      System.out.println( args[0] + " " + result );
      s.decode();
   }
}
