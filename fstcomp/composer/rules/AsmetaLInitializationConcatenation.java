package composer.rules;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;

import de.ovgu.cide.fstgen.ast.FSTNode;
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
