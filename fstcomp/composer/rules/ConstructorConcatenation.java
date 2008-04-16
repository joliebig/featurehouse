package composer.rules;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class ConstructorConcatenation implements CompositionRule {

	public void compose(FSTTerminal terminalA, FSTTerminal terminalB, FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
		String constructorA = terminalA.getBody();
		String constructorB = terminalB.getBody();
		constructorB = constructorB.substring(0, constructorB.lastIndexOf("}"));
		constructorA = constructorA.substring(constructorA.indexOf("{") + 1, constructorA.length());
		
		terminalComp.setBody(constructorB + constructorA);
	}
}
