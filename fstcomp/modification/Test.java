/**
 * 
 */
package modification;

import java.io.File;
import java.io.FileNotFoundException;

import modification.FSTParseables.StringInput;

/**
 * @author Boxleitner Stefan
 * 
 */
public class Test {

    /**
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws FileNotFoundException,
	    Exception {

	System.out
		.println(FSTGenerator
			.createFSTParseable(
				new StringInput(
					"void print(){System.out.print( \"edge (\" );a.print();System.out.print( \", \" );b.print();System.out.print( \") \" );}",
					"java.method")).getFST());
	System.out.println(FSTGenerator
		.createFST(new File("modification/test")));
    }
}
