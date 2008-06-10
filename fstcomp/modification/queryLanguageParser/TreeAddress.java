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
	//TODO BUG ahhhhhhhhhhhh!
	List<FSTNode> possibleNodesOld = new LinkedList<FSTNode>();
	List<FSTNode> possibleNodesNew = new LinkedList<FSTNode>();
	List<FSTNode> resultNodes = new LinkedList<FSTNode>();
	possibleNodesOld.add(compilation);
	TreeAddressToken token;

	Iterator<TreeAddressToken> it = addressTokens.iterator();
	while (it.hasNext()) {
	    token = it.next();
	    possibleNodesNew.clear();
	    for (FSTNode node : possibleNodesOld) {
		if (token.isMatchWithNode(node)) {
		    if (token.getPossibleMatchingChildren(node) != null)
			possibleNodesNew.addAll(token
				.getPossibleMatchingChildren(node));

		    if (!it.hasNext())
			resultNodes.add(node);

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