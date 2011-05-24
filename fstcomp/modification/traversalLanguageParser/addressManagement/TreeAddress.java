/**
 * 
 */
package modification.traversalLanguageParser.addressManagement;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * @author Boxleitner Stefan
 * 
 */
public class TreeAddress {
    private List<TreeAddressToken> addressTokens = new LinkedList<TreeAddressToken>();

    public TreeAddress(List<TreeAddressToken> addressTokens) {
	this.addressTokens.addAll(addressTokens);
    }

    /**
     * 
     */
    public List<FSTNode> resolve(FSTNode compilation) {
	List<FSTNode> possibleNodesOld = new DuplicateFreeLinkedList<FSTNode>();
	List<FSTNode> possibleNodesNew = new DuplicateFreeLinkedList<FSTNode>();
	List<FSTNode> resultNodes = new DuplicateFreeLinkedList<FSTNode>();
	possibleNodesOld.add(compilation);
	TreeAddressToken token;

	Iterator<TreeAddressToken> it = addressTokens.iterator();
	while (it.hasNext()) {
	    token = it.next();
	    possibleNodesNew.clear();
	    for (FSTNode node : possibleNodesOld) {
		if (token.getMatchingNodes(node)!=null) {
		    if (token.getPossibleMatchingFollowUps(node) != null
			    && !possibleNodesNew.contains(node)) {
			possibleNodesNew.addAll(token
				.getPossibleMatchingFollowUps(node));
		    }
		    if (!it.hasNext()) {
			resultNodes.addAll(token.getMatchingNodes(node));

		    }
		}
	    }

	    possibleNodesOld.clear();
	    possibleNodesOld.addAll(possibleNodesNew);

	}
	return resultNodes;
    }

    /*
     * 
     */
    public List<TreeAddressToken> getAddressParts() {
	return addressTokens;
    }
}