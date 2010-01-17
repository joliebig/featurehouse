package modification.content;

import java.io.FileNotFoundException;

import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;

public interface Content {

    public FSTNode getFST() throws FileNotFoundException, ParseException,
	    modification.traversalLanguageParser.ParseException,
	    InvalidFSTTraversalException;

}
