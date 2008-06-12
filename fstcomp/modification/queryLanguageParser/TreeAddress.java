/**
 * 
 */
package modification.queryLanguageParser;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

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
	    System.out.println("TOKEN: " + token);
	    possibleNodesNew.clear();
	    for (FSTNode node : possibleNodesOld) {
		System.out.println("  NODE: " + node.getName() + " : "
			+ node.getType());
		if (token.getMatchingNodes(node)!=null) {
		    System.out.println("  MATCH!");
		    if (token.getPossibleMatchingFollowUps(node) != null
			    && !possibleNodesNew.contains(node)) {
			possibleNodesNew.addAll(token
				.getPossibleMatchingFollowUps(node));
			System.out.println("    ADDED possible CHILDREN: "
				+ node.getName() + " : " + node.getType());
		    }
		    if (!it.hasNext()) {
			resultNodes.addAll(token.getMatchingNodes(node));
			System.out.println("    ADDED to RESULT: "
				+ node.getName() + " : " + node.getType());

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