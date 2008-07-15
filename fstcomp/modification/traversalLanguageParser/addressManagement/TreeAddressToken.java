/**
 * 
 */
package modification.traversalLanguageParser.addressManagement;

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
    public abstract List<FSTNode> getPossibleMatchingFollowUps(FSTNode node);

    /**
     * 
     */
    public abstract List<FSTNode> getMatchingNodes(FSTNode node);

}
