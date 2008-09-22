/**
 * 
 */
package modification;

import java.io.FileNotFoundException;

import modification.content.Content;
import modification.content.InvalidFSTTraversalException;
import modification.traversalLanguageParser.ParseException;
import modification.traversalLanguageParser.TraversalLanguageParser;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

/**
 * @author Boxleitner Stefan
 * 
 */
public class IntroductionModification extends ContentModification {

    /**
     * 
     * @param fstTraversal
     * @param content
     */
    public IntroductionModification(String fstTraversal, Content content) {
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
	for (FSTNode node : tlp.parse())
	    ((FSTNonTerminal) node).addChild(
		    getContent()
		    .getFST());
    }

}
