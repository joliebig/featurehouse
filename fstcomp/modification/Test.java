/**
 * 
 */
package modification;

import java.io.File;

import modification.FSTParseables.FSTParseable;
import modification.FSTParseables.FileInput;
import modification.FSTParseables.JavaFile;
import modification.FSTParseables.JavaMethod;
import modification.FSTParseables.StringInput;

/**
 * @author Boxleitner Stefan
 * 
 */
public class Test {

    /**
     * @param args
     * 
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
	FSTParseable jf = new JavaFile(new FileInput(new File(
		"examples/Modification/Edge.java")));
	System.out.println(jf.parseToFST());

	FSTParseable jt = new JavaMethod(
		new StringInput(
			"void print(){System.out.print( \"edge (\" );a.print();System.out.print( \", \" );b.print();System.out.print( \") \" );}"));
	System.out.println(jt.parseToFST());

	System.out.println(FileFSTGenerator.createFSTRecursive(new File(
		"modification/test")));
    }
}
