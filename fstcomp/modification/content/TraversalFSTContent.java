package modification.content;

import java.io.FileNotFoundException;
import java.util.List;

import modification.traversalLanguageParser.ParseException;
import modification.traversalLanguageParser.TraversalLanguageParser;
import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * content, given by an address to a content directory in the filesystem and an
 * FST traversal to select the desired artifact, i.e. an FST node
 * 
 * @author Boxleitner Stefan
 */
public class TraversalFSTContent implements Content {

    /**
     * @param fstTraversal
     * @param root
     */
    public TraversalFSTContent(String fstTraversal, Content content) {
	super();
	this.fstTraversal = fstTraversal;
	this.content = content;
    }

    /**
     * 
     */
    private Content content;

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
		fstTraversal, content.getFST());
	List<FSTNode> list = tparser.parse();
	if (list.size() == 1) {
	    return list.get(0);
	} else {
	    throw new InvalidFSTTraversalException();
	}
    }
}
