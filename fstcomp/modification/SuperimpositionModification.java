/**
 * 
 */
package modification;

import java.io.FileNotFoundException;

import composer.FSTGenComposer;

import modification.traversalLanguageParser.ParseException;
import modification.traversalLanguageParser.TraversalLanguageParser;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

/**
 * @author Boxleitner Stefan
 * @uml.dependency supplier="modification.Content"
 */
public class SuperimpositionModification extends Modification {

    /**
     * 
     * @param fstTraversal
     * @param content
     */
    public SuperimpositionModification(String fstTraversal, Content content) {
	super(fstTraversal, content);
    }

    /*
     * (non-Javadoc)
     * 
     * @see modification.Modification#apply(de.ovgu.cide.fstgen.ast.FSTNode)
     */
    @Override
    public void apply(FSTNode root) throws ParseException,
	    FileNotFoundException, cide.gparser.ParseException,
	    InvalidFSTTraversalException {
	TraversalLanguageParser tlp = new TraversalLanguageParser(
		getFstTraversal(), root);
	for (FSTNode node : tlp.parse()) {
	    ((FSTNonTerminal) node.getParent()).addChild(FSTGenComposer
		    .compose(node, getContent().getFST()));
	    ((FSTNonTerminal) node.getParent()).removeChild(node);
	}
    }
}
