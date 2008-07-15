/**
 * 
 */
package modification.traversalLanguageParser.addressManagement;

import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

/*
 * 
 */
public class NameTypePatternToken extends TreeAddressToken {

    private String namePattern;

    private String typePattern;

    /**
     * 
     */
    @Override
    public List<FSTNode> getPossibleMatchingFollowUps(FSTNode node) {
	if (node instanceof FSTNonTerminal)
	    return ((FSTNonTerminal) node).getChildren();
	else
	    return null;
    }

    /**
     * 
     */
    @Override
    public List<FSTNode> getMatchingNodes(FSTNode node) {
	List<FSTNode> list = new LinkedList<FSTNode>();
	if (Pattern.matches(namePattern, node.getName())
		&& Pattern.matches(typePattern, node.getType())) {
	    list.add(node);
	    return list;
	}
	return null;
    }

    /**
     * 
     */
    public NameTypePatternToken(String namePattern, String typePattern) {
	this.namePattern = namePattern;
	this.typePattern = typePattern;
    }

    public String toString() {
	return ("namePattern: " + namePattern + " typePattern: " + typePattern);
    }
}
