package modification;

import java.io.FileNotFoundException;
import java.util.LinkedList;

import modification.content.InvalidFSTTraversalException;
import modification.traversalLanguageParser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;

public class ModificationComposition extends LinkedList<Modification> {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /**
     * apply every single modification
     * 
     * @throws ParseException
     * @throws cide.gparser.ParseException
     * @throws InvalidFSTTraversalException
     * @throws FileNotFoundException
     * @throws UnknownTypeParseException
     */
    public void apply(FSTNode root) throws ParseException,
	    FileNotFoundException, cide.gparser.ParseException,
	    InvalidFSTTraversalException {
	for (Modification mod : this) {
	    mod.apply(root);
	}
    }

}
