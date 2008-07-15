/**
 * 
 */
package modification.traversalLanguageParser.addressManagement;

import java.util.LinkedList;
import java.util.List;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;
import de.ovgu.cide.fstgen.ast.FSTVisitor;

/**
 * @author Boxleitner Stefan
 * 
 */
public class LogIterator extends FSTVisitor {
    private List<FSTNode> iteratorHistory = new LinkedList<FSTNode>();

    public LogIterator() {
	super();
    }

    public boolean visit(FSTTerminal terminal) {
	iteratorHistory.add(terminal);
	return true;
    }

    public boolean visit(FSTNonTerminal nonTerminal) {
	iteratorHistory.add(nonTerminal);
	return true;
    }

    /**
     * 
     * @return
     */
    public List<FSTNode> getLog() {
	return iteratorHistory;
    }

}
