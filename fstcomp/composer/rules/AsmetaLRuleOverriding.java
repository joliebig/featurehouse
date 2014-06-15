package composer.rules;

import java.util.StringTokenizer;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class AsmetaLRuleOverriding extends AbstractCompositionRule {
    public final static String COMPOSITION_RULE_NAME = "AsmetaLRuleOverriding";

    public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
	    FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) 
    {    	
    	if (terminalA.getBody().contains("@original"))
    	{
    		String newBodyA = terminalA.getBody().replace("@original", terminalA.getName() + "_" + terminalA.getFeatureName() + "__wrapee__");
    		
    		String newBodyB = terminalB.getBody();
    		//Delete main token
    		newBodyB = newBodyB.substring(newBodyB.indexOf("rule"));
    		
    		int posEqual = newBodyB.indexOf("=") == -1 ? Integer.MAX_VALUE : newBodyB.indexOf("="); 
    		int posBracket = newBodyB.indexOf("(") == -1 ? Integer.MAX_VALUE : newBodyB.indexOf("(");
    		String ruleName = newBodyB.substring(newBodyB.indexOf("r_"), Math.min(posEqual, posBracket));
    		newBodyB = newBodyB.replace(ruleName, terminalA.getName() + "_" + terminalA.getFeatureName() + "__wrapee__");
    		newBodyB += "\n";    		
    		terminalComp.setBody(newBodyB + newBodyA);    		
    	}    	
		
    }

    private static String getFeatureName(FSTNode node) {
	if (node.getType().equals("Feature"))
	    return node.getName();
	else
	    return getFeatureName(node.getParent());
    }
}
