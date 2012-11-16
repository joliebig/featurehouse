package composer.rules.rtcomp.java;

import composer.rules.AbstractCompositionRule;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class JavaRuntimeReplacement extends AbstractCompositionRule {

	@Override
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
		// do nothing
		
	}

	@Override
	public String getRuleName() {		
		return "Replacement";
	}

}
