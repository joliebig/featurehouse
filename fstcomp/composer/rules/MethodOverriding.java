package composer.rules;

import java.util.StringTokenizer;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class MethodOverriding {
	public final static String COMPOSITION_RULE_NAME = "MethodOverriding";
	public static void compose(FSTTerminal terminalA, FSTTerminal terminalB, FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
		String toReplace = "original\\s*\\(";
		if(terminalA.getBody().matches(".*\\s*original\\s*.*")){
			FSTTerminal terminalComp2 = (FSTTerminal) terminalB.getDeepClone();
			nonterminalParent.addChild(terminalComp2);

			String oldMethodName = terminalB.getName();

			StringTokenizer st = new StringTokenizer(oldMethodName, "(");
			if(st.hasMoreTokens())
				oldMethodName = st.nextToken();
			st = new StringTokenizer(oldMethodName, " ");
			
			while(st.hasMoreTokens())
				oldMethodName = st.nextToken();

			String newMethodName = oldMethodName + "__wrappee__" + getFeatureName(terminalB);
			String newBody = terminalComp.getBody().replaceAll(toReplace, newMethodName + "(");
			terminalComp.setBody(newBody);

			terminalComp2.setBody(terminalComp2.getBody().replaceFirst(oldMethodName, newMethodName));
			terminalComp2.setName(newMethodName);
		}
	}
	private static String getFeatureName(FSTNode node) {
		if(node.getType().equals("Feature"))
			return node.getName();
		else
			return getFeatureName(node.getParent());
	}
}
