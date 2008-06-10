/**
 * 
 */
package modification.queryLanguageParser;

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
    public List<FSTNode> getPossibleMatchingChildren(FSTNode node) {
	LogIterator logIt = new LogIterator();
	node.accept(logIt);
	List<FSTNode> list = logIt.getLog();
	list.remove(node);
	return list;
    }

    /**
     * 
     */
    @Override
    public boolean isMatchWithNode(FSTNode node) {
	return true;
    }
}