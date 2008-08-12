package modification;

import java.io.FileNotFoundException;

import modification.traversalLanguageParser.ParseException;
import modification.traversalLanguageParser.TraversalLanguageParser;
import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * content, given by an address to a content directory in the filesystem and an
 * FST traversal to select the desired artifact, i.e. an FST node
 * 
 * @author Boxleitner Stefan
 */
public class ParsedTraversalFSTContent implements Content {

    /**
     * @param fstTraversal
     * @param root
     */
    public ParsedTraversalFSTContent(String fstTraversal, FSTNode root) {
	super();
	this.fstTraversal = fstTraversal;
	this.root = root;
    }

    /**
     * 
     */
    private FSTNode root;

    /**
     * FST traversal to select the desired artifact, i.e. an FST node
     */
    private String fstTraversal;

    /*
     * (non-Javadoc)
     * 
     * @see modification.xmlParser.Content#getContent()
     */
    @Override
    public FSTNode getFST() throws ParseException,
	    InvalidFSTTraversalException, FileNotFoundException,
	    cide.gparser.ParseException {
	TraversalLanguageParser tparser = new TraversalLanguageParser(
		fstTraversal, root);
	if (tparser.parse().size() == 1)
	    return tparser.parse().get(0);
	else
	    throw new InvalidFSTTraversalException();
    }
}
