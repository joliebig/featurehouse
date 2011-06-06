
package org.gjt.sp.jedit.syntax;

import javax.swing.text.Segment;


public class SyntaxUtilities
{
	
	
	public static boolean regionMatches(boolean ignoreCase, Segment text,
		int offset, char[] match)
	{
		int length = offset + match.length;
		if(length > text.offset + text.count)
			return false;
		char[] textArray = text.array;
		for(int i = offset, j = 0; i < length; i++, j++)
		{
			char c1 = textArray[i];
			char c2 = match[j];
			if(ignoreCase)
			{
				c1 = Character.toUpperCase(c1);
				c2 = Character.toUpperCase(c2);
			}
			if(c1 != c2)
				return false;
		}
		return true;
	} 
}
