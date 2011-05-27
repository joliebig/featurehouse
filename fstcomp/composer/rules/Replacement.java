package composer.rules;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class Replacement extends AbstractCompositionRule {

	@Override
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
		
		System.out.println("Terminal replacement: " +
				 terminalA.toString() + " replaces " +
				 terminalB.toString());
		
		// do nothing - the work has already been done in FSTGenComposer
	}
	public final static String COMPOSITION_RULE_NAME = FSTTerminal.defaultCompositionMechanism;
}
