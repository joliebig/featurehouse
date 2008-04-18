package composer.rules;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class StringConcatenation {
	public final static String COMPOSITION_RULE_NAME = "StringConcatenation";
	public static void compose(FSTTerminal terminalA, FSTTerminal terminalB, FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
		terminalComp.setBody(terminalB.getBody() + terminalA.getBody());
	}
}
