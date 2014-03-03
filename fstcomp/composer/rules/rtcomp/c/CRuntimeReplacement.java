package composer.rules.rtcomp.c;

import composer.rules.AbstractCompositionRule;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class CRuntimeReplacement  extends AbstractCompositionRule {

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
