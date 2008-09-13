/**
 * 
 */
package modification;

import java.io.FileNotFoundException;

import modification.content.Content;
import modification.content.InvalidFSTTraversalException;
import modification.traversalLanguageParser.ParseException;
import modification.traversalLanguageParser.TraversalLanguageParser;

import composer.FSTGenComposer;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

/**
 * @author Boxleitner Stefan
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
	    // TODO catch null pointer
	    if (FSTGenComposer.compose(getContent().getFST(), node, node
		    .getParent()) == null) {
		System.out.println("====== error print ======");
		System.out.println(getContent().getFST());
		System.out.println();
		System.out.println(node);
		System.out.println("====== error print end ======");
	    }
	}
    }
}
