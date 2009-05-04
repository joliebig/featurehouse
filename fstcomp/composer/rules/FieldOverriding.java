package composer.rules;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class FieldOverriding {
	public final static String COMPOSITION_RULE_NAME = "FieldOverriding";
	public static void compose(FSTTerminal terminalA, FSTTerminal terminalB, FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
		String bodyA = terminalA.getBody();
		String bodyB = terminalB.getBody();
		String compBody;

		if(!bodyB.contains(",") && !bodyA.contains(",")) {

			if(bodyB.contains("=")) {
				if(bodyA.contains("="))
					compBody = bodyA;
				else
					compBody = bodyB;
			} else {
				compBody = bodyA;
			}
			terminalComp.setBody(compBody);
		} else {
			System.err.println("Error: compound field declarations cannot be composed: \'" + bodyA + "\' and \'" + bodyB + "\'");
		}
	}
}
