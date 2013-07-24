package composer.rules.meta;

import composer.FSTGenComposerExtension;
import composer.rules.AbstractCompositionRule;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

/**
 * TODO description
 * @author Jens Meinicke
 *
 */
public class InvariantCompositionMeta extends AbstractCompositionRule {

	public final static String COMPOSITION_RULE_NAME = "InvariantComposition";
	
	@Override
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {

	}
	
	@Override
	public void preCompose(FSTTerminal terminalA) {
		String body = terminalA.getBody();
		body = body.replaceAll("invariant ", "invariant FM.FeatureModel." + getFeatureName(terminalA) + " ==> (");
		body = body.replaceAll(";", ");");
		terminalA.setBody(body);
	}

	private static String getFeatureName(FSTNode node) {
		if (node.getType().equals("Feature"))
			return node.getName() + (FSTGenComposerExtension.key ? "" : "()");
		else
			return getFeatureName(node.getParent()).toLowerCase();
	}
	
	@Override
	public String getRuleName() {
		return COMPOSITION_RULE_NAME;
	}
}
