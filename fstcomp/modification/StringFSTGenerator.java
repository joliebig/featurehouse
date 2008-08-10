package modification;

import java.io.FileNotFoundException;

import modification.FSTParseables.JavaFile;
import modification.FSTParseables.JavaMethod;
import modification.FSTParseables.StringInput;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class StringFSTGenerator {
    public static FSTNode createFST(String content, String type)
	    throws FileNotFoundException, ParseException {
	if (type.equals("java.file")) {
	    return new JavaFile(new StringInput(content)).parseToFST();
	} else if (type.equals("java.file")) {
	    return new JavaMethod(new StringInput(content)).parseToFST();
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
	// }
	else {
	    return new FSTTerminal("unknown type", "", "", "");
	}
    }
}
