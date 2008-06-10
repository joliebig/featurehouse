/**
 * 
 */
package modification.queryLanguageParser;

import java.util.List;

import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * @author Boxleitner Stefan
 * 
 */
public abstract class TreeAddressToken {
    /**
     * 
     */
    public abstract List<FSTNode> getPossibleMatchingChildren(FSTNode node);

    /**
     * 
     */
    public abstract boolean isMatchWithNode(FSTNode node);

}
