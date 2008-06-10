/**
 * 
 */
package modification.queryLanguageParser;

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
    public List<FSTNode> getPossibleMatchingChildren(FSTNode node) {
	if (node.getClass() == FSTNonTerminal.class)
	    return ((FSTNonTerminal) node).getChildren();
	else
	    return null;
    }

    /**
     * 
     */
    @Override
    public boolean isMatchWithNode(FSTNode node) {
	if (Pattern.matches(namePattern, node.getName())
		&& Pattern.matches(typePattern, node.getType()))
	    return true;
	return false;
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
