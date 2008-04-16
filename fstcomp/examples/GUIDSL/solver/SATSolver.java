// 
// test calling the minisolver

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;

import org.sat4j.minisat.SolverFactory;
import org.sat4j.minisat.core.SolverStats;
import org.sat4j.reader.*;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.TimeoutException;
import java.io.*;

/* this layer is not yet complete.  It is possible to call a solver
   directly from a LineNumberReader.  I've started the implementation
	but haven't completed it yet.  At present, _debug.cnf files are
	generated but not cleaned up.  Here's the help message from Daniel
	Le Berre:

You can provide a LineNumberReader to DimacsReader.parseInstance().

ISolver solver = SolverFactory.newMiniLearning();
DimacsReader reader = new DimacsReader(solver);

LineNumberReader in = new LineNumberReader(new BufferedReader(...));

reader.parseInstance(in);

*/

public class SATSolver {

   ISolver solver;
   InstanceReader reader;
   DimacsReader dreader;
	boolean isSat;

   SATSolver() {
      solver = SolverFactory.createSolverByName("MiniSAT");
		// initialize both, although only one will be used
      reader = new InstanceReader(solver);
      dreader = new DimacsReader(solver);
   }

   // argument needs a LineNumberReader as input
   // returns result of SAT solver
   //
   public boolean solve( LineNumberReader in ) {
      isSat = false;
      try {
         dreader.parseInstance(in);
	      isSat = solver.isSatisfiable();
      } catch (ContradictionException e) {
	    isSat = false;
      } catch (TimeoutException e) {
            System.out.println("Timeout");
	    System.exit(1);
      } catch (ParseFormatException e) {
	    System.out.println("Parse Format Exception");
            e.printStackTrace();
	    System.exit(1);
      } catch (Exception e) {
	    System.out.println("CNF file format likely wrong");
	    e.printStackTrace();
	    System.exit(1);
      }

      return isSat;
   }
   // argument needs a cnf file type as input
   // returns result of SAT solver
   //
   public boolean solve( String filename ) {
      isSat = false;
      try {
         reader.parseInstance(filename);
	      isSat = solver.isSatisfiable();
      }
      catch (FileNotFoundException e) {
	    System.out.println("File Not found " + filename);
            e.printStackTrace();
	    System.exit(1);
      } catch (IOException e) {
	    System.out.println("IOException " + e.getMessage());
            e.printStackTrace();
	    System.exit(1);
      } catch (ContradictionException e) {
	    isSat = false;
      } catch (TimeoutException e) {
            System.out.println("Timeout");
	    System.exit(1);
      } catch (ParseFormatException e) {
	    System.out.println("Parse Format Exception");
            e.printStackTrace();
	    System.exit(1);
      } catch (Exception e) {
	    System.out.println("CNF file format likely wrong");
	    e.printStackTrace();
	    System.exit(1);
      }

      return isSat;
   }

   void decode() {
	   if (isSat) {
         int[] model = solver.model();
         System.out.println("v "+reader.decode(model));

         System.out.println( "Solutions");
			for (int i = 0; i<model.length; i++ ) {
			   if (model[i] > 0) System.out.println(model[i]);
		   }
		}
	}
}
