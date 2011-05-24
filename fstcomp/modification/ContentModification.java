package modification;

import java.io.FileNotFoundException;

import modification.content.Content;
import modification.content.InvalidFSTTraversalException;
import modification.traversalLanguageParser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * @author Boxleitner Stefan
 * 
 */
public abstract class ContentModification implements Modification {

    /**
     * content of the modification
     */
    private Content content;

    /**
     * traversal to select nodes in the FST to modify
     */
    private String fstTraversal;

    /**
     * @param fstTraversal
     *            traversal to select nodes in the FST to modify
     * @param content
     *            content of the modification
     */
    public ContentModification(String fstTraversal, Content content) {
	super();
	this.fstTraversal = fstTraversal;
	this.content = content;
    }

    /* (non-Javadoc)
     * @see modification.Modification#apply(de.ovgu.cide.fstgen.ast.FSTNode)
     */
    public abstract void apply(FSTNode root) throws ParseException,
	    FileNotFoundException, cide.gparser.ParseException,
	    InvalidFSTTraversalException;

    /**
     * @return the content
     */
    public Content getContent() {
	return content;
    }

    /* (non-Javadoc)
     * @see modification.Modification#getFstTraversal()
     */
    public String getFstTraversal() {
	return fstTraversal;
    }
}
