package composer.rules;

import java.util.StringTokenizer;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class CSharpMethodOverriding {
	public final static String COMPOSITION_RULE_NAME = "CSharpMethodOverriding";
	public static void compose(FSTTerminal terminalA, FSTTerminal terminalB, FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
		
		if(terminalA.getBody().matches(".*\\s*original\\s*.*")){
			FSTTerminal terminalComp2 = (FSTTerminal) terminalB.getDeepClone();
			FSTNonTerminal grandParent = (FSTNonTerminal)nonterminalParent.getParent();
			FSTNonTerminal newParent = (FSTNonTerminal)nonterminalParent.getShallowClone();
			newParent.addChild(terminalComp2);
			grandParent.addChild(newParent);
			
			String oldMethodName = terminalB.getName();
			
			StringTokenizer st = new StringTokenizer(oldMethodName, "(");
			if(st.hasMoreTokens()) {
				oldMethodName = st.nextToken();
			}
			st = new StringTokenizer(oldMethodName, " ");

			while(st.hasMoreTokens()) {
				oldMethodName = st.nextToken();
			}
			
			String toReplace = "original\\s*\\(";
			String newMethodName = oldMethodName + "__wrappee__" + getFeatureName(terminalB);
			String newBody = terminalComp.getBody().replaceAll(toReplace, newMethodName + "(");
			terminalComp.setBody(newBody);

			String auxBody = "";
			st = new StringTokenizer(terminalComp2.getBody(), "(");
			if(st.hasMoreTokens()) {
				auxBody = st.nextToken();
			}
			
			st = new StringTokenizer(auxBody.trim(), " ");
			String prefix = "";
			boolean found = false;
			while(st.hasMoreTokens() && !found) {
				String token = st.nextToken();
				if(oldMethodName.equals(token)) {
					found = true;
				} else {
					prefix += token + " ";
				}
			}
			
			terminalComp2.setBody(prefix + terminalComp2.getBody().replaceFirst(prefix, "").replaceFirst(oldMethodName, newMethodName));
			terminalComp2.setName(newMethodName);
			newParent.setName(newMethodName);
		}
	}
	private static String getFeatureName(FSTNode node) {
		if(node.getType().equals("Feature"))
			return node.getName();
		else
			return getFeatureName(node.getParent());
	}
}
