package composer.rules;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class StringConcatenation extends AbstractCompositionRule {
	public final static String COMPOSITION_RULE_NAME = "StringConcatenation";
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB, FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
		terminalComp.setBody(terminalB.getBody() + terminalA.getBody());
	}
	@Override
	public String getRuleName() {
		return COMPOSITION_RULE_NAME;
	}
}
