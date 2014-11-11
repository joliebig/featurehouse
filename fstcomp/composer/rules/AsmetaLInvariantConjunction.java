package composer.rules;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class AsmetaLInvariantConjunction extends AbstractCompositionRule 
{
    public final static String COMPOSITION_RULE_NAME = "AsmetaLInvariantConjunction";
    
    private List<String> unsplitBracketedStrings(List<String> l)
    {
    	List<String> output = new LinkedList<String>();    	
		int openedSubBracket = 0;
		String object = "";
		for(String a : l)
		{
			if (a.contains("(")) openedSubBracket++;
			if (a.contains(")")) openedSubBracket--;
			object += a;
			if (openedSubBracket == 0)
			{
				output.add(object);
				object = "";
			}
		}    	
    	return output;
    }

    public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
	    FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) 
    {   
    	if (terminalB.getBody().contains("@final_invariant"))
    	{
    		//THROW ERROR
    		return;
    	}
    	if (terminalA.getBody().contains("@conjunct") && 
    		terminalA.getBody().contains("@original"))
    	{
    		//THROW ERROR
    		return;
    	}
    	String newHeader = "";
    	if (terminalA.getBody().contains("@conjunct") ||
    		terminalA.getBody().contains("@original"))
    	{
    		//Combine Header
    		List<String> headerA = Arrays.asList(terminalA.getBody().substring(terminalA.getBody().indexOf("over") + 4, terminalA.getBody().indexOf(":")).replace("\\s+", "").split(","));
    		List<String> headerB = Arrays.asList(terminalB.getBody().substring(terminalB.getBody().indexOf("over") + 4, terminalB.getBody().indexOf(":")).replace("\\s+", "").split(","));    		
    		headerA = unsplitBracketedStrings(headerA);
    		headerB = unsplitBracketedStrings(headerB);    		
    		Set<String> newHeaderSet = new TreeSet<String>(headerA);
    		newHeaderSet.addAll(headerB);
    		for (String h : newHeaderSet)
    		{
    			newHeader += h + ", "; 
    		}
    		newHeader = newHeader.substring(0, newHeader.length() - 2);    		
    	}
    	if (terminalA.getBody().contains("@conjunct"))
    	{
    		String newBody = terminalA.getBody();
    		newBody = newBody.substring(newBody.indexOf(":") + 1);
    		newBody = "(" + newBody + ") and (";
    		newBody += terminalB.getBody().substring(terminalB.getBody().indexOf(":") + 1) + ")";    	
    		//Keep Annotations from terminalA - such as final_invariant
    		newBody = terminalA.getBody().substring(0, terminalA.getBody().indexOf("over") + 4) + newHeader + ":" + newBody;    		
    		terminalComp.setBody(newBody);
    	}
    	if (terminalA.getBody().contains("@original"))
    	{
    		String bodyA = terminalA.getBody();
    		bodyA = bodyA.substring(bodyA.indexOf(":") + 1);
    		String bodyB = terminalB.getBody();
    		bodyB = bodyB.substring(bodyB.indexOf(":") + 1);
    		String newBody = bodyA.replace("@original", " (" + bodyB + ")");
    		newBody = terminalA.getBody().substring(0, terminalA.getBody().indexOf("over") + 4) + newHeader + ":" + newBody;    		
    		terminalComp.setBody(newBody);
    	}    	
		
    }
}
