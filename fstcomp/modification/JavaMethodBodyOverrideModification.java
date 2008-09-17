/**
 * 
 */
package modification;

import java.io.FileNotFoundException;

import modification.content.InvalidFSTTraversalException;
import modification.content.Parseables.java.JavaMethodBody;
import modification.traversalLanguageParser.ParseException;
import modification.traversalLanguageParser.TraversalLanguageParser;

import composer.FSTGenComposer;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

/**
 * @author boxleitner
 * 
 */
public class JavaMethodBodyOverrideModification extends
	SuperimpositionModification {

    public JavaMethodBodyOverrideModification(String fstTraversal,
	    JavaMethodBody body) {
	super(fstTraversal, body);
    }

    @Override
    public void apply(FSTNode root) throws ParseException,
	    FileNotFoundException, cide.gparser.ParseException,
	    InvalidFSTTraversalException {
	TraversalLanguageParser tlp = new TraversalLanguageParser(
		getFstTraversal(), root);	

	for (FSTNode node : tlp.parse()) {
	    FSTTerminal contentFST = (FSTTerminal) getContent().getFST();
	    contentFST.setName(node.getName());
	    contentFST.setType(node.getType());
	    contentFST.setCompositionMechanism(((FSTTerminal) node)
		    .getCompositionMechanism());
	    String newBody = ((FSTTerminal) node).getBody().split("[{]")[0];	    
	    newBody = newBody + contentFST.getBody();
	    contentFST.setBody(newBody);	    
	    
	    // TODO catch null pointer
	    FSTNode composedNode = FSTGenComposer.compose(contentFST, node,
		    node.getParent());	    
	    ((FSTNonTerminal) node.getParent()).addChild(composedNode);
	    ((FSTNonTerminal) node.getParent()).removeChild(node);
	}
    }
}
