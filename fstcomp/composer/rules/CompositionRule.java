package composer.rules;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public interface CompositionRule {
	void compose(FSTTerminal nodeA, FSTTerminal nodeB, FSTTerminal terminalComp, FSTNonTerminal nonterminalParent);
}
