package composer.rules.meta;

import composer.FSTGenComposerExtension;
import composer.rules.FieldOverriding;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

/**
 * TODO description
 * @author Jens Meinicke
 *
 */
public class FieldOverridingMeta extends FieldOverriding {
	
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB, FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
		specializeModifiers(terminalA, terminalB);

		String bodyA = terminalA.getBody();
		String bodyB = terminalB.getBody();
		String compBody;

		if(!bodyB.contains(",") && !bodyA.contains(",")) {
			if(bodyB.contains("=")) {
				if(bodyA.contains("="))
					compBody = compose(terminalA, terminalB, terminalComp, bodyA, bodyB);
				else
					compBody = bodyB;
			} else {
				compBody = bodyA;
			}
			terminalComp.setBody(compBody);
		} else {
			// TODO implement this can happen for alternative features
			System.err.println("Error: compound field declarations cannot be composed: \'" + bodyA + "\' and \'" + bodyB + "\'");
		}

	}

	private String compose(FSTTerminal terminalA, FSTTerminal terminalB, FSTTerminal terminalComp, String bodyA, String bodyB) {
		String fieldName = bodyB.substring(0,bodyB.indexOf('=')).trim();
		fieldName = fieldName.substring(fieldName.lastIndexOf(' '));
//		String fieldA = bodyA.substring(bodyA.indexOf('=') + 1).replace(";", "").trim();
		
//		FSTTerminal invariant = findInvariant("#" + fieldName, terminalComp);
//		if (invariant != null) {
//			String oldInvariant = invariant.getBody().substring(invariant.getBody().indexOf("FM.FeatureModel.") - 2, invariant.getBody().length() - 1);
//			String newInavariant = "invariant (FM.FeatureModel." + getFeatureName(terminalA) + " ==> " + fieldName + " == " + fieldA + 
//					") && (!FM.FeatureModel." + getFeatureName(terminalA) + " ==> (" + oldInvariant + ");";
//			invariant.setBody(newInavariant);
//		} else if (bodyA.contains("final ")) {
//			String fieldB = bodyB.substring(bodyB.indexOf('=') + 1).replace(";", "").trim();
//			String prefix = " ==> (" + fieldName + " == " + fieldB + ")";
//			String newPrefix = "invariant (FM.FeatureModel." + getFeatureName(terminalA) + " ==> " + fieldName + " == " + fieldA + 
//					") && (!FM.FeatureModel." + getFeatureName(terminalA) + prefix + ");";
//			if (bodyA.contains("static ")) {
//				newPrefix = "static " + newPrefix;
//			}
//			FSTNonTerminal decl = new FSTNonTerminal("ClassOrInterfaceBodyDeclaration1", "auto7");
//			((FSTNonTerminal)((FSTNonTerminal) terminalComp.getParent()).getParent()).addChild(decl);
//			FSTNonTerminal jml = new FSTNonTerminal("JMLDeclaration1", "#" + fieldName);
//			decl.addChild(jml);
//			jml.addChild(new FSTTerminal("Invariant", "-", newPrefix, "", "InvariantComposition"));
//		}
		return bodyA.substring(0, bodyB.indexOf('=') + 1) + " FM.FeatureModel." + getFeatureName(terminalA) +
				   " ? " + bodyA.substring(bodyA.indexOf('=') + 1).replaceAll(";", "") + " : " + bodyB.substring(bodyB.indexOf('=') + 1);
	}
	
//	/**
//	 * TODO description
//	 * @param string
//	 * @param terminalComp
//	 * @return
//	 */
//	private FSTTerminal findInvariant(String string, FSTTerminal terminalComp) {
//		FSTNonTerminal fieldDecl = ((FSTNonTerminal)terminalComp.getParent());
//		FSTNonTerminal classDeclaration = ((FSTNonTerminal)fieldDecl.getParent());
//		for (FSTNode child : classDeclaration.getChildren()) {
//			if (child instanceof FSTNonTerminal) {
//				FSTNonTerminal nonTerminal = (FSTNonTerminal) child;
//				if (nonTerminal.getType().equals("ClassOrInterfaceBodyDeclaration1")) {
//					FSTNonTerminal jMLDeclaration = (FSTNonTerminal)nonTerminal.getChildren().get(0);
//					if (jMLDeclaration.getName().equals(string)) {
//						return (FSTTerminal)(jMLDeclaration).getChildren().get(0);
//					}
//				}
//			}
//		}
//		return null;
//	}

	private static String getFeatureName(FSTNode node) {
		if (node.getType().equals("Feature"))
			return node.getName().toLowerCase() + (FSTGenComposerExtension.key ? "" : "()");
		else
			return getFeatureName(node.getParent());
	}

	@Override
	public String getRuleName() {
		return COMPOSITION_RULE_NAME;
	}
}
