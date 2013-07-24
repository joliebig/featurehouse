package composer.rules.meta;

import composer.FSTGenComposerExtension;
import composer.rules.ConstructorConcatenation;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class ConstructorConcatenationMeta extends ConstructorConcatenation {

//	@Override
//	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
//			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
//		if (FSTGenComposerExtension.key) {
//			super.compose(terminalA, terminalB, terminalComp, nonterminalParent);
//			return;
//		}
//		specializeModifiers(terminalA, terminalB);
//		
//		String constructorA = terminalA.getBody();
//		String constructorB = terminalB.getBody();
//		constructorB = constructorB.substring(0, constructorB.lastIndexOf("}"));
//		constructorA = constructorA.substring(constructorA.indexOf("{") + 1, constructorA.lastIndexOf("}"));
//		
//		terminalComp.setBody(constructorB.replaceAll("//entry", constructorA) + "\r\n\t}");
//	}
	
	@Override
	public void preCompose(FSTTerminal terminalA) {
		String constructorA = terminalA.getBody();
		String head = constructorA.substring(0, constructorA.indexOf("{") + 1);
		String body = constructorA.substring(constructorA.indexOf("{") + 1, constructorA.lastIndexOf("}"));
		body = body.replaceAll("\\n", "\n\t");
		terminalA.setBody(head + "\r\n\t\tif (FM.FeatureModel." + getFeatureName(terminalA).toLowerCase() + ") {" + body + "\t\t}\r\n\t}");
//		terminalA.setBody(head + "\r\n\t\tif (FeatureModel." + getFeatureName(terminalA).toLowerCase() + ") {" + body + "\r\n\t\t//entry\r\n\t\t} else {\r\n\t\t//entry\r\n\t\t}\r\n\t}");
	}
	
	private static String getFeatureName(FSTNode node) {
		if (node.getType().equals("Feature")) {
			return node.getName() + (FSTGenComposerExtension.key ? "" : "()");
		} else {
			return getFeatureName(node.getParent());
		}
	}

	@Override
	public String getRuleName() {
		return COMPOSITION_RULE_NAME;
	}
}
