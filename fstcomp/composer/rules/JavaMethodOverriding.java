package composer.rules;

import java.util.StringTokenizer;
import java.util.regex.Pattern;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class JavaMethodOverriding {
    public final static String COMPOSITION_RULE_NAME = "JavaMethodOverriding";

    public static void compose(FSTTerminal terminalA, FSTTerminal terminalB,
	    FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
	if (terminalA.getBody().matches("(?s).*\\s*original\\s*.*")) {
	    FSTTerminal terminalComp2 = (FSTTerminal) terminalB.getDeepClone();
	    nonterminalParent.addChild(terminalComp2);

	    String oldMethodName = terminalB.getName();

	    StringTokenizer st = new StringTokenizer(oldMethodName, "(");
	    if (st.hasMoreTokens()) {
		oldMethodName = st.nextToken();
	    }
	    st = new StringTokenizer(oldMethodName, " ");

	    while (st.hasMoreTokens()) {
		oldMethodName = st.nextToken();
	    }

	    String toReplace = "original\\s*\\(";
	    String newMethodName = oldMethodName + "__wrappee__"
		    + getFeatureName(terminalB);
	    String newBody = terminalComp.getBody().replaceAll(toReplace,
		    newMethodName + "(");
	    terminalComp.setBody(newBody);

	    String auxBody = "";
	    st = new StringTokenizer(terminalComp2.getBody(), "(");
	    if (st.hasMoreTokens()) {
		auxBody = st.nextToken();
	    }

	    st = new StringTokenizer(auxBody, " ");
	    String prefix = "";
	    boolean found = false;
	    while (st.hasMoreTokens() && !found) {
		String token = st.nextToken();
		if (oldMethodName.equals(token)) {
		    found = true;
		} else {
		    prefix += token + " ";
		}
	    }
	    	    
	    // Replace '[]' for regex
	    String modPrefix = "";
	    for(char c:prefix.toCharArray()){
		if (c=='[')
		    modPrefix += "\\[";
		else if (c==']')
		    modPrefix += "\\]";
		else
		    modPrefix += String.valueOf(c);
	    }
	    
	    prefix=modPrefix;	    
	    
	    terminalComp2.setBody(prefix
		    + terminalComp2.getBody().replaceFirst(prefix, "")
			    .replaceFirst(oldMethodName, newMethodName));
	    terminalComp2.setName(newMethodName);
	}
    }

    private static String getFeatureName(FSTNode node) {
	if (node.getType().equals("Feature"))
	    return node.getName();
	else
	    return getFeatureName(node.getParent());
    }
}
