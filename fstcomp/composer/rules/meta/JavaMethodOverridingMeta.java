package composer.rules.meta;

import java.util.List;

import composer.CompositionException;
import composer.FSTGenComposerExtension;
import composer.rules.JavaMethodOverriding;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

/**
 * Extends the standard {@link JavaMethodOverriding} to generate a meta product.
 * Adds return to the original method if the corresponding feature is not selected.
 * 
 * @author Jens Meinicke
 *
 */
public class JavaMethodOverridingMeta extends JavaMethodOverriding {
	
	/**
	 * TODO description
	 */
	@Override
	protected boolean replaceOriginal(FSTTerminal terminalA) {
		return true;
	}
	
	@Override
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) throws CompositionException {
		super.compose(terminalA, terminalB, terminalComp, nonterminalParent);
		
		if (!super.replaceOriginal(terminalA)) {
			// all sub methods will not exist when this feature is selected
			// TODO experimental
			// dont do this if generating runtime assertions  
			List<FSTNode> children = ((FSTNonTerminal)nonterminalParent.getParent()).getChildren();
			for (FSTNode child : children) {
				if (child.getType().equals("MethodDeclarationWithSpec")) {
					FSTTerminal terminal = (FSTTerminal)((FSTNonTerminal)child).getChildren().get(2);
					if (terminal.getName().equals(terminalA.getName())) {
						setNotFeature(child, getFeatureName(terminalA));
					}
				}
			}
		}
	}
	
	public static String getFeatureName(FSTNode node) {
		if (node.getType().equals("Feature"))
			return node.getName().toLowerCase();
		else
			return getFeatureName(node.getParent());
	}
	
	private void setNotFeature(FSTNode child, String featureName) {
		FSTNonTerminal methodSpecification = (FSTNonTerminal)((FSTNonTerminal)child).getChildren().get(0);
		FSTNonTerminal specification = (FSTNonTerminal)((FSTNonTerminal)methodSpecification).getChildren().get(0);
		FSTTerminal specCaseSeq = (FSTTerminal)((FSTNonTerminal)specification).getChildren().get(0);
		
		int ensuresIndex = specCaseSeq.getBody().indexOf("ensures ");
		int requiresIndex = specCaseSeq.getBody().indexOf("requires ");
		int index = ensuresIndex < requiresIndex ? ensuresIndex : requiresIndex;
		if (ensuresIndex == -1) {
			if (requiresIndex == -1) {
				index = 0;
			} else {
				index = requiresIndex;
			}
		} else if (requiresIndex == -1) {
			index = ensuresIndex;
		}
		
		String start = specCaseSeq.getBody().substring(0, index);
		String end = specCaseSeq.getBody().substring(index);
		featureName = featureName + (FSTGenComposerExtension.key ? "" : "()"); 
		specCaseSeq.setBody(start + "\r\n\t @ \\nreq !FM.FeatureModel." + featureName + ";\r\n\t @ " + end);	
	}

	@Override
	protected String getNewBody(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, String oldMethodName) {
		return addEvasion(terminalA, terminalB, terminalComp, oldMethodName);
	}
	
	/**
	 * Adds the usage of the refined method if the feature is not selected.
	 * @param terminalA
	 * @param terminalB
	 * @param terminalComp // TODO replace with Terminal A or B
	 * @param name 
	 * @return
	 */
	private String addEvasion(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, String name) {
		String parameterNames = getParameterNames(terminalA);
		String returnType = getReturnType(terminalA.getBody(), name);
		return terminalComp.getBody().replaceFirst("\\{", "{\n\t\tif (!FM.FeatureModel."+ getLowFeatureName(terminalA) +
				(returnType.isEmpty() ? ") {\n\t\t\toriginal("+parameterNames+");\n\t\t\treturn;\n\t\t}" : 
				")\n\t\t\treturn original("+ parameterNames +");"));
	}

	private String getParameterNames(FSTTerminal terminalA) {
		String parameter = terminalA.getBody().substring(
				terminalA.getBody().indexOf('(') + 1, terminalA.getBody().indexOf(')')).trim();
		String parameterNames = "";
		String[] p = parameter.split("[,]");
		for (int i = 0; i < p.length; i++) {
			String[] split = p[i].trim().split("[ ]");
			if (split.length < 2) {
				continue;
			}
			parameterNames += split[split.length-1];
			if (i < p.length - 1) {
				parameterNames += ", ";
			}
		}
		return parameterNames;
	}

	public String getReturnType(String body, String name) {
		// remove @annotations
		body = body.replaceAll("@\\w*\\W*", "");
		// remove spaces between name and ()
		body = body.replaceAll(name + "\\W*\\(", name + "(");
		body =  body.substring(0, body.indexOf(name + "("));
		body = body.split("[ ]")[body.split("[ ]").length -1];
		body = body.replaceAll(" ", "");
		if (body.equals("void")) {
			return "";
		}
		return body;
	}
	
	private String getLowFeatureName(FSTNode node) {
		return getFeatureName(node).toLowerCase() + (FSTGenComposerExtension.key ? "" : "()");
	}

}
