/**
 * 
 */
package modification.traversalLanguageParser.addressManagement;

import java.util.List;

import de.ovgu.cide.fstgen.ast.FSTNode;

/*
 * 
 */
public class WildcardToken extends TreeAddressToken {

    /**
     * 
     */
    @Override
    public List<FSTNode> getPossibleMatchingFollowUps(FSTNode node) {
	return subTreeList(node);
    }

    /**
     * 
     */
    @Override
    public List<FSTNode> getMatchingNodes(FSTNode node) {
	return subTreeList(node);
    }

    private List<FSTNode> subTreeList(FSTNode node) {	
	LogIterator logIt = new LogIterator();
	node.accept(logIt);
	List<FSTNode> list = logIt.getLog();
	return list;
    }
}