/**
 * 
 */
package modification;

import java.io.File;
import java.io.FileNotFoundException;

import modification.FSTParseables.FSTParseable;
import modification.FSTParseables.FileInput;
import modification.FSTParseables.Input;
import modification.FSTParseables.JavaFile;
import modification.FSTParseables.JavaMethod;
import modification.FSTParseables.UnknownType;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

/**
 * generates an FST out of a given root file
 * 
 * @author Boxleitner Stefan
 */
public class FSTGenerator {
    /*
     * returns all files in a given directory (recursive)
     */
    public static FSTNode createFST(File file) throws FileNotFoundException,
	    ParseException {
	if (file.isDirectory()) {
	    FSTNonTerminal folder = new FSTNonTerminal("folder", file
		    .toString());
	    for (File f : file.listFiles()) {
		folder.addChild(createFST(f));
	    }
	    return folder;
	} else if (file.isFile())
	    return createFSTParseable(new FileInput(file)).getFST();
	else
	    return null;
    }

    public static FSTParseable createFSTParseable(Input in)
	    throws FileNotFoundException, ParseException {
	if (in.getType().equals("java") || in.getType().equals("java.file")) {
	    return new JavaFile(in);
	} else if (in.getType().equals("java.method")) {
	    return new JavaMethod(in);
	}
	// else if (in.getType().equals("java.method")) {
	// return new JavaMethod(in);
	// }
	else {
	    return new UnknownType(in);
	}
    }
}
