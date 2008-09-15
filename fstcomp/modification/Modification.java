package modification;

import java.io.FileNotFoundException;

import modification.content.InvalidFSTTraversalException;
import modification.traversalLanguageParser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;

public interface Modification {

    /**
     * applies the modification to an existing FST
     * 
     * @param root
     *            root of the FST to modify
     * @throws ParseException
     * @throws cide.gparser.ParseException
     * @throws InvalidFSTTraversalException
     * @throws FileNotFoundException
     * @throws UnknownTypeParseException
     */
    public abstract void apply(FSTNode root) throws ParseException,
	    FileNotFoundException, cide.gparser.ParseException,
	    InvalidFSTTraversalException;

}