

package org.gjt.sp.jedit.indent;


public abstract class BracketIndentRule implements IndentRule
{
	
	public BracketIndentRule(char openBracket, char closeBracket)
	{
		this.openBracket = openBracket;
		this.closeBracket = closeBracket;
	} 

	
	public static class Brackets
	{
		int openCount;
		int closeCount;
	} 

	
	public Brackets getBrackets(String line)
	{
		Brackets brackets = new Brackets();

		for(int i = 0; i < line.length(); i++)
		{
			char ch = line.charAt(i);
			if(ch == openBracket)
			{
				
				if(line.length() - i >= 3)
				{
					if(line.substring(i,i+3).equals("{{{")) 
					{
						i += 2;
						continue;
					}
				}
				brackets.openCount++;
			}
			else if(ch == closeBracket)
			{
				if(brackets.openCount != 0)
					brackets.openCount--;
				else
					brackets.closeCount++;
			}
		}

		return brackets;
	} 

	
	public String toString()
	{
		return getClass().getName() + "[" + openBracket + ","
			+ closeBracket + "]";
	} 

	protected char openBracket, closeBracket;
}
