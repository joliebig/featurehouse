package composer.rules;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.StringTokenizer;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class AsmetaLRuleOverriding extends AbstractCompositionRule {
    public final static String COMPOSITION_RULE_NAME = "AsmetaLRuleOverriding";

    
    class IntStr
    {
    	int pos;
    	String token;
    }
    
    class StrStrMap
    {
    	String bodyRemainder;
    	HashMap<String, String> statements;
    }    
    
    public IntStr nextToken(String body, String[] tokens)
    {
    	IntStr next = new IntStr();
    	next.pos = Integer.MAX_VALUE;
    	next.token = null;
    	for (String t : tokens)
    	{    		
    		if (body.indexOf(t) != -1 && body.indexOf(t) < next.pos)
    		{
    			next.token = t;
    			next.pos = body.indexOf(t);
    		}
    	}
    	return next;
    }
    
    public StrStrMap splitStatements(String begin, String end, String separator, String finalAlternativeSeparator, String nameSeparator, String body)
    {
    	String curBody = body;
		curBody = curBody.substring(curBody.indexOf(separator));		
    	String subStatement;
    	HashMap<String, String> subStatementList = new HashMap<String, String>();
		curBody = curBody.substring(separator.length());    	
    	boolean isLastStatement = false;
    	while (isLastStatement == false)
    	{
    		subStatement = "";
    		int depth = 0;
    		do
    		{
    			IntStr tok = nextToken(curBody, new String[]{begin, end, separator, finalAlternativeSeparator});
    			subStatement += curBody.substring(0, curBody.indexOf(tok.token) + tok.token.length());
    			curBody = curBody.substring(curBody.indexOf(tok.token) + tok.token.length());    			
    			if (tok.token.equals(begin) == true)
    			{
    				depth++;
    			}
    			if (depth > 0)
    			{
    				if (tok.token.equals(separator) == true || tok.token.equals(finalAlternativeSeparator) == true)
    				{
    					
    				}
        			if (tok.token.equals(end) == true)
        			{
        				depth --;
        			}
    			}else
    			{
    				//Depth == 0 && next token is separator or end
        			if (tok.token.equals(separator) == true || tok.token.equals(finalAlternativeSeparator) == true)
        			{
        				subStatement = subStatement.substring(0, subStatement.lastIndexOf(tok.token));
        				break;
        			}
        			if (tok.token.equals(end) == true)
        			{
        				subStatement = subStatement.substring(0, subStatement.lastIndexOf(tok.token));
        				isLastStatement = true;
        				break;
        			}
    			}
    		} while(true);
    		String id = subStatement.substring(0, subStatement.indexOf(nameSeparator));
    		subStatementList.put(id, subStatement);    		
    	}    
    	StrStrMap out = new StrStrMap();
    	out.bodyRemainder = curBody;
    	out.statements = subStatementList;
    	return out;
    }
    
    public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
	    FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) 
    {   
    	if (terminalB.getBody().contains("@final_rule"))
    	{
    		//THROW ERROR!
    		terminalComp.setBody(terminalB.getBody());
    		return;
    	}
    	while (terminalA.getBody().contains("@extend_original"))
    	{
    		if (terminalB.getBody().contains("@extendable"))
    		{
    			String bodyA = terminalA.getBody();
    			String bodyB = terminalB.getBody();    			
    			//Check what statement is being extended    			
    			String termA = bodyA.substring(bodyA.indexOf("/*@extend_original#"));
    			String subtermID = termA.substring(19);
    			subtermID = subtermID.substring(0, subtermID.indexOf("*/"));
    			int statementPos = termA.indexOf(" ") + 1;
    			int startPos = statementPos + bodyA.indexOf("/*@extend_original#");
    			String statementA = termA.substring(statementPos).trim();
    			statementA = statementA.substring(0, statementA.indexOf(" "));
    			HashMap<String, String> subStatementsA = null;
    			HashMap<String, String> subStatementsB = null;
    			StrStrMap A = new StrStrMap();
    			StrStrMap B = new StrStrMap();
    			
    			//Handle Switch statements
    			if (statementA.compareTo("switch") == 0)
    			{
    				A = splitStatements("switch", "endswitch", "case", "otherwise", ":", bodyA.substring(startPos));
    				subStatementsA = A.statements;    				
    				B = splitStatements("switch", "endswitch", "case", "otherwise", ":", bodyB.substring(bodyB.indexOf("extendable#" + subtermID)));
    				subStatementsB = B.statements;
    			}
    			////INSERT FUTURE STATEMENTS HERE/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    			//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////    			
    			if (statementA.compareTo("StatementXYZ") == 0)
    			{
    				A = splitStatements("beginXYZ", "endXYZ", "separatorXYZ", "otherwiseXYZ", ":", bodyA.substring(statementPos));
    				subStatementsA = A.statements;    				
    				B = splitStatements("beginXYZ", "endXYZ", "separatorXYZ", "otherwiseXYZ", ":", bodyB.substring(bodyB.indexOf(subtermID)));
    				subStatementsB = B.statements;    				
    			}
    			//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////    			
    			//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    			//No Statement applied
    			if (subStatementsA == null || subStatementsB == null)
    			{
    				//THROW ERROR
    				return;
    			}
    			
    			
    			//Compose substatements
    			for (String ssA : subStatementsA.keySet())
    			{
    				if (subStatementsA.get(ssA).contains("@compose#" + subtermID) == true || subStatementsA.get(ssA).contains("@original#" + subtermID))
    				{
    					//Compose
    					String newStatement = subStatementsA.get(ssA);
    					String subStatementBWithoutBegin = subStatementsB.get(ssA);
    					subStatementBWithoutBegin = subStatementBWithoutBegin.substring(subStatementBWithoutBegin.indexOf(":") + 1);
    					newStatement = newStatement.replace("@original#" + subtermID, subStatementBWithoutBegin);
    					newStatement = newStatement.replace("/*@compose#" + subtermID + "*/", "");
    					subStatementsA.put(ssA, newStatement);
    				}    			
    			}
    			for (String ssB : subStatementsB.keySet())
    			{
    				if (subStatementsA.containsKey(ssB) == false)
    				{
    					subStatementsA.put(ssB, subStatementsB.get(ssB));
    				}
    			}
    			
    			
    			//Reinsert composed substatement into superstructure
    			String newbodyA = bodyA.substring(0, bodyA.indexOf("/*@extend_original"));    			
    			if (statementA.compareTo("switch") == 0)
    			{
    				String temp = bodyA.substring(startPos);
    				IntStr next = nextToken(temp, new String[]{"case", "otherwise"});
    				newbodyA += temp.substring(0, next.pos);
    				
        			for (String ssA : subStatementsA.keySet())
        			{
        				if (ssA.equals(":"))
        				{
        					newbodyA += "otherwise";	
        				}else
        				{
        					newbodyA += "case";	
        				}        				
        				newbodyA += subStatementsA.get(ssA);
        			}
        			newbodyA += "\nendswitch";	
    			}    			
    			newbodyA += A.bodyRemainder;    			
    			terminalA.setBody(newbodyA);
    			terminalComp.setBody(newbodyA);
    		}else
    		{
    			//THROW ERROR: Unallowed extension
        		terminalComp.setBody(terminalB.getBody());
        		return;
    		}
    	}
    		
    	if (terminalA.getBody().contains("@original"))
    	{
    		if (terminalA.getBody().startsWith("main"))
    		{
    			String newBodyB = terminalB.getBody().substring(terminalB.getBody().indexOf("=") + 1);
    			String newBodyA = terminalA.getBody().replace("@original[]", newBodyB).replace("@original()", newBodyB);
    			terminalComp.setBody(newBodyA);
    		}else
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
		
    }
}
