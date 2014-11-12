package composer.rules;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class AsmetaLInitializationConcatenation extends AbstractCompositionRule 
{
    public final static String COMPOSITION_RULE_NAME = "AsmetaLInitializationConcatenation";
    


    public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
	    FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) 
    {    	
    	String bodyA = terminalA.getBody();
    	String bodyB = terminalB.getBody();
    	bodyB = bodyB.substring(bodyB.indexOf(":") + 1);    	
    	bodyA = bodyA.replace("@original()", bodyB);    	
    	terminalComp.setBody(bodyA);    	
    }
}
