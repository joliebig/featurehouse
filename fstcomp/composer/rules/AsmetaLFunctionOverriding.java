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

public class AsmetaLFunctionOverriding extends AbstractCompositionRule 
{
    public final static String COMPOSITION_RULE_NAME = "AsmetaLFunctionOverriding";
    
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

    	while (terminalA.getBody().contains("@original"))
    	{
    		boolean hasParameters = false;
    		String newBodyB = terminalB.getBody().substring(terminalB.getBody().indexOf("=") + 1);
			String afterOriginal = terminalA.getBody().substring(terminalA.getBody().indexOf("@original"));			
			afterOriginal = afterOriginal.replace("\\s+", "");
			if (afterOriginal.indexOf("(") == 9)
			{
				hasParameters = true;
				String parametersNew = afterOriginal.substring(10, afterOriginal.indexOf(")"));
				String parametersOld = terminalB.getBody().substring(0, terminalB.getBody().indexOf("="));				
				if (parametersOld.indexOf("(") == -1)
				{
					//THROW ERROR
					return;
				}
				parametersOld = parametersOld.substring(parametersOld.indexOf("(") + 1, parametersOld.indexOf(")"));
				String pOld[] = parametersOld.split(",");
				for (int i = 0; i < pOld.length; i++)
				{
					pOld[i] = pOld[i].substring(pOld[i].indexOf("$"), pOld[i].indexOf(" in"));
				}
				String pNew[] = parametersNew.split(",");
				
				for (int i = 0; i < pNew.length; i++)
				{
					newBodyB = newBodyB.replace(pOld[i], pNew[i]);
				}
			}
			int originalIndex = terminalA.getBody().indexOf("@original");
			String newBodyA = terminalA.getBody();
			newBodyA = newBodyA.replace("@original", "");
			if (hasParameters)
			{			
				String postOriginal = terminalA.getBody().substring(originalIndex);
				postOriginal = postOriginal.substring(postOriginal.indexOf(")") + 1);
				newBodyA = newBodyA.substring(0, originalIndex) + postOriginal;
			}
			newBodyA = newBodyA.substring(0, originalIndex) + "(" + newBodyB + ")" + newBodyA.substring(originalIndex);
			terminalA.setBody(newBodyA);
    	}
    	terminalComp.setBody(terminalA.getBody());    	
    }
}
