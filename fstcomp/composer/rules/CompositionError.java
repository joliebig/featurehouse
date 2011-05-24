package composer.rules;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class CompositionError {
	public final static String COMPOSITION_RULE_NAME = "CompositionError";
	public static void compose(FSTTerminal terminalA, FSTTerminal terminalB, FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
		System.err.println("Composition error: " + terminalA.getName() + " cannot be composed with " + terminalB.getName());
	}
}
