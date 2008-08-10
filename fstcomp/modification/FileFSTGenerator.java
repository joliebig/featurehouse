/**
 * 
 */
package modification;

import java.io.File;
import java.io.FileNotFoundException;

import modification.FSTParseables.FileInput;
import modification.FSTParseables.JavaFile;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

/**
 * generates an FST out of a given root file
 * 
 * @author Boxleitner Stefan
 */
public class FileFSTGenerator {
    /*
     * returns all files in a given directory (recursive)
     */
    public static FSTNode createFSTRecursive(File file)
	    throws FileNotFoundException, ParseException {
	System.out.println(file.getAbsolutePath());
	if (file.isDirectory()) {
	    FSTNonTerminal folder = new FSTNonTerminal("folder", file
		    .toString());
	    for (File f : file.listFiles()) {
		folder.addChild(createFSTRecursive(f));
	    }
	    return folder;
	} else if (file.isFile())
	    return createFST(file);
	else
	    return null;
    }

    private static FSTNode createFST(File file)
	    throws FileNotFoundException, ParseException {
	if (file.getName().endsWith(".java")) {
	    return new JavaFile(new FileInput(file)).parseToFST();
	}
	// else if (file.getName().endsWith(".java")) {
	//
	// } else if (file.getName().endsWith(".java")) {
	//
	// } else if (file.getName().endsWith(".java")) {
	//
	// } else if (file.getName().endsWith(".java")) {
	//
	// } else if (file.getName().endsWith(".java")) {
	//
	// } else if (file.getName().endsWith(".java")) {
	//
	// } else if (file.getName().endsWith(".java")) {
	//
	// } else if (file.getName().endsWith(".java")) {
	//
	// } else if (file.getName().endsWith(".java")) {
	//
	// }
	else {
	    return new FSTTerminal("unknown type", file.getName(), "", "");
	}
    }
}
