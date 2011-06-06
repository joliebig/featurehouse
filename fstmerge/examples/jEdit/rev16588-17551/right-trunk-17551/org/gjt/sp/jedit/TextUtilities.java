

package org.gjt.sp.jedit;


import java.util.*;
import javax.swing.text.Segment;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.syntax.*;
import org.gjt.sp.util.StandardUtilities;



public class TextUtilities
{
	
	public static final int BRACKET_MATCH_LIMIT = 10000;

	
	
	public static Token getTokenAtOffset(Token tokens, int offset)
	{
		if(offset == 0 && tokens.id == Token.END)
			return tokens;

		for(;;)
		{
			if(tokens.id == Token.END)
				throw new ArrayIndexOutOfBoundsException("offset > line length");

			if(tokens.offset + tokens.length > offset)
				return tokens;
			else
				tokens = tokens.next;
		}
	} 

	
	
	public static char getComplementaryBracket(char ch, boolean[] direction)
	{
		switch(ch)
		{
		case '(': if (direction != null) direction[0] = true;  return ')';
		case ')': if (direction != null) direction[0] = false; return '(';
		case '[': if (direction != null) direction[0] = true;  return ']';
		case ']': if (direction != null) direction[0] = false; return '[';
		case '{': if (direction != null) direction[0] = true;  return '}';
		case '}': if (direction != null) direction[0] = false; return '{';
		case '<': if (direction != null) direction[0] = true;  return '>';
		case '>': if (direction != null) direction[0] = false; return '<';
		default:  return '\0';
		}
	} 

	
	
	public static int findMatchingBracket(JEditBuffer buffer, int line, int offset)
	{
		if(offset < 0 || offset >= buffer.getLineLength(line))
		{
			throw new ArrayIndexOutOfBoundsException(offset + ":"
				+ buffer.getLineLength(line));
		}

		Segment lineText = new Segment();
		buffer.getLineText(line,lineText);

		char c = lineText.array[lineText.offset + offset];
		
		boolean[] direction = new boolean[1];

		
		char cprime = getComplementaryBracket(c,direction);

		if( cprime == '\0' )
		{ 
			return -1;
		}

		
		int count = 1;

		DefaultTokenHandler tokenHandler = new DefaultTokenHandler();
		buffer.markTokens(line,tokenHandler);

		
		
		
		byte idOfBracket = getTokenAtOffset(tokenHandler.getTokens(),offset).id;

		boolean haveTokens = true;

		int startLine = line;

		
		if(direction[0])
		{
			offset++;

			for(;;)
			{
				for(int i = offset; i < lineText.count; i++)
				{
					char ch = lineText.array[lineText.offset + i];
					if(ch == c)
					{
						if(!haveTokens)
						{
							tokenHandler.init();
							buffer.markTokens(line,tokenHandler);
							haveTokens = true;
						}
						if(getTokenAtOffset(tokenHandler.getTokens(),i).id == idOfBracket)
							count++;
					}
					else if(ch == cprime)
					{
						if(!haveTokens)
						{
							tokenHandler.init();
							buffer.markTokens(line,tokenHandler);
							haveTokens = true;
						}
						if(getTokenAtOffset(tokenHandler.getTokens(),i).id == idOfBracket)
						{
							count--;
							if(count == 0)
								return buffer.getLineStartOffset(line) + i;
						}
					}
				}

				
				line++;
				if(line >= buffer.getLineCount() || (line - startLine) > BRACKET_MATCH_LIMIT)
					break;
				buffer.getLineText(line,lineText);
				offset = 0;
				haveTokens = false;
				
			}
		} 
		
		else
		{
			offset--;

			for(;;)
			{
				for(int i = offset; i >= 0; i--)
				{
					char ch = lineText.array[lineText.offset + i];
					if(ch == c)
					{
						if(!haveTokens)
						{
							tokenHandler.init();
							buffer.markTokens(line,tokenHandler);
							haveTokens = true;
						}
						if(getTokenAtOffset(tokenHandler.getTokens(),i).id == idOfBracket)
							count++;
					}
					else if(ch == cprime)
					{
						if(!haveTokens)
						{
							tokenHandler.init();
							buffer.markTokens(line,tokenHandler);
							haveTokens = true;
						}
						if(getTokenAtOffset(tokenHandler.getTokens(),i).id == idOfBracket)
						{
							count--;
							if(count == 0)
								return buffer.getLineStartOffset(line) + i;
						}
					}
				}

				
				line--;
				if(line < 0 || (startLine - line) > BRACKET_MATCH_LIMIT)
					break;
				buffer.getLineText(line,lineText);
				offset = lineText.count - 1;
				haveTokens = false;
				
			}
		} 

		
		return -1;
	} 

	
	
	public static int findWordStart(String line, int pos, String noWordSep)
	{
		return findWordStart(line, pos, noWordSep, true, false);
	} 

	
	
	public static int findWordStart(CharSequence line,
					int pos,
					String noWordSep)
	{
		return findWordStart(line, pos, noWordSep, true, false, false);
	} 


	
	public static String join(Collection<String> c, String delim)
	{
		StringBuilder retval = new StringBuilder();
		Iterator<String> itr = c.iterator();
		if (itr.hasNext())
			retval.append( itr.next() );
		else
			return "";
		while (itr.hasNext())
		{
			retval.append(delim);
			retval.append(itr.next());
		}
		return retval.toString();
	}

	
	
	public static int findWordStart(String line, int pos, String noWordSep,
		boolean joinNonWordChars)
	{
		return findWordStart(line,pos,noWordSep,joinNonWordChars,false);
	} 

	
	
	public static int findWordStart(String line, int pos, String noWordSep,
		boolean joinNonWordChars, boolean eatWhitespace)
	{
		return findWordStart(line, pos, noWordSep, joinNonWordChars, false, eatWhitespace);
	} 

	
	
	public static int findWordStart(String line, int pos, String noWordSep,
		boolean joinNonWordChars, boolean camelCasedWords,
		boolean eatWhitespace)
	{
		return findWordStart((CharSequence) line, pos, noWordSep,
				     joinNonWordChars, camelCasedWords,
				     eatWhitespace);
	} 

	
	
	public static int findWordStart(CharSequence line,
					int pos,
					String noWordSep,
					boolean joinNonWordChars,
					boolean camelCasedWords,
					boolean eatWhitespace)
	{
		return findWordStart(line, pos, noWordSep, joinNonWordChars, camelCasedWords, eatWhitespace, false);
	} 

	
	
	public static int findWordStart(CharSequence line, int pos, String noWordSep,
		boolean joinNonWordChars, boolean camelCasedWords,
		boolean eatWhitespace, boolean eatOnlyAfterWord)
	{
		char ch = line.charAt(pos);

		if(noWordSep == null)
			noWordSep = "";

		
		int type = getCharType(ch, noWordSep);
		

		for(int i = pos; i >= 0; i--)
		{
			char lastCh = ch;
			ch = line.charAt(i);
			switch(type)
			{
			
			case WHITESPACE:
				
				if(Character.isWhitespace(ch))
					break;
				
				else if (!eatOnlyAfterWord)
				{
					return i + 1;
				}
				
				else if (Character.isLetterOrDigit(ch) || noWordSep.indexOf(ch) != -1)
				{
					type = WORD_CHAR;
				}
				else
					type = SYMBOL;
				break; 
			
			case WORD_CHAR:
				
				
				if (camelCasedWords && Character.isUpperCase(ch) && !Character.isUpperCase(lastCh)
						&& Character.isLetterOrDigit(lastCh))
				{
					return i;
				}
				
				
				else if (camelCasedWords && !Character.isUpperCase(ch) && Character.isUpperCase(lastCh))
				{
					return i + 1;
				}
				
				else if(Character.isLetterOrDigit(ch) ||
					noWordSep.indexOf(ch) != -1)
				{
					break;
				}
				
				else if(Character.isWhitespace(ch)
					&& eatWhitespace && !eatOnlyAfterWord)
				{
					type = WHITESPACE;
					break;
				}
				else
					return i + 1; 
			
			case SYMBOL:
				if(!joinNonWordChars && pos != i)
					return i + 1;

				
				if(Character.isWhitespace(ch))
				{
					if(eatWhitespace && !eatOnlyAfterWord)
					{
						type = WHITESPACE;
						break;
					}
					else
						return i + 1;
				}
				else if(Character.isLetterOrDigit(ch) ||
					noWordSep.indexOf(ch) != -1)
				{
					return i + 1;
				}
				else
				{
					break;
				} 
			}
		}

		return 0;
	} 

	
	
	public static int findWordEnd(String line, int pos, String noWordSep)
	{
		return findWordEnd(line, pos, noWordSep, true);
	} 

	
	
	public static int findWordEnd(CharSequence line,
				      int pos,
				      String noWordSep)
	{
		return findWordEnd(line, pos, noWordSep, true, false, false);
	} 

	
	
	public static int findWordEnd(String line, int pos, String noWordSep,
		boolean joinNonWordChars)
	{
		return findWordEnd(line,pos,noWordSep,joinNonWordChars,false);
	} 

	
	
	public static int findWordEnd(String line, int pos, String noWordSep,
		boolean joinNonWordChars, boolean eatWhitespace)
	{
		return findWordEnd(line, pos, noWordSep, joinNonWordChars, false, eatWhitespace);
	} 

	
	
	public static int findWordEnd(String line, int pos, String noWordSep,
		boolean joinNonWordChars, boolean camelCasedWords,
		boolean eatWhitespace)
	{
		return findWordEnd((CharSequence)line, pos, noWordSep,
				   joinNonWordChars, camelCasedWords,
				   eatWhitespace);
	} 

	
	
	public static int findWordEnd(CharSequence line,
				      int pos,
				      String noWordSep,
				      boolean joinNonWordChars,
				      boolean camelCasedWords,
				      boolean eatWhitespace)
	{
		if(pos != 0)
			pos--;

		char ch = line.charAt(pos);

		if(noWordSep == null)
			noWordSep = "";

		
		int type = getCharType(ch, noWordSep);
		

		for(int i = pos; i < line.length(); i++)
		{
			char lastCh = ch;
			ch = line.charAt(i);
			switch(type)
			{
			
			case WHITESPACE:
				
				if(Character.isWhitespace(ch))
					break;
				else
					return i; 
			
			case WORD_CHAR:
				
				
				if (camelCasedWords && i > pos + 1 && !Character.isUpperCase(ch) && Character.isLetterOrDigit(ch)
						&& Character.isUpperCase(lastCh))
				{
					return i - 1;
				}
				
				else if (camelCasedWords && Character.isUpperCase(ch) && !Character.isUpperCase(lastCh))
				{
					return i;
				}
				else if(Character.isLetterOrDigit(ch) ||
					noWordSep.indexOf(ch) != -1)
				{
					break;
				}
				
				else if(Character.isWhitespace(ch)
					&& eatWhitespace)
				{
					type = WHITESPACE;
					break;
				}
				else
					return i; 
			
			case SYMBOL:
				if(!joinNonWordChars && i != pos)
					return i;

				
				if(Character.isWhitespace(ch))
				{
					if(eatWhitespace)
					{
						type = WHITESPACE;
						break;
					}
					else
						return i;
				}
				else if(Character.isLetterOrDigit(ch) ||
					noWordSep.indexOf(ch) != -1)
				{
					return i;
				}
				else
				{
					break;
				} 
			}
		}

		return line.length();
	} 

	
	private static int getCharType(char ch, String noWordSep)
	{
		int type;
		if(Character.isWhitespace(ch))
			type = WHITESPACE;
		else if(Character.isLetterOrDigit(ch)
			|| noWordSep.indexOf(ch) != -1)
			type = WORD_CHAR;
		else
			type = SYMBOL;
		return type;
	}

	
	
	public static String spacesToTabs(String in, int tabSize)
	{
		StringBuilder buf = new StringBuilder();
		int width = 0;
		int whitespace = 0;
		for(int i = 0; i < in.length(); i++)
		{
			switch(in.charAt(i))
			{
			case ' ':
				whitespace++;
				width++;
				break;
			case '\t':
				int tab = tabSize - (width % tabSize);
				width += tab;
				whitespace += tab;
				break;
			case '\n':
				if(whitespace != 0)
				{
					buf.append(StandardUtilities
						.createWhiteSpace(whitespace,tabSize,
						width - whitespace));
				}
				whitespace = 0;
				width = 0;
				buf.append('\n');
				break;
			default:
				if(whitespace != 0)
				{
					buf.append(StandardUtilities
						.createWhiteSpace(whitespace,tabSize,
						width - whitespace));
					whitespace = 0;
				}
				buf.append(in.charAt(i));
				width++;
				break;
			}
		}

		if(whitespace != 0)
		{
			buf.append(StandardUtilities.createWhiteSpace(whitespace,tabSize,
				width - whitespace));
		}

		return buf.toString();
	} 

	
	
	public static String tabsToSpaces(String in, int tabSize)
	{
		StringBuilder buf = new StringBuilder();
		int width = 0;
		for(int i = 0; i < in.length(); i++)
		{
			switch(in.charAt(i))
			{
			case '\t':
				int count = tabSize - (width % tabSize);
				width += count;
				while(--count >= 0)
					buf.append(' ');
				break;
			case '\n':
				width = 0;
				buf.append(in.charAt(i));
				break;
			default:
				width++;
				buf.append(in.charAt(i));
				break;
			}
		}
		return buf.toString();
	} 

	
	
	public static String format(String text, int maxLineLength, int tabSize)
	{
		StringBuilder buf = new StringBuilder();

		int index = 0;

		for(;;)
		{
			int newIndex = text.indexOf("\n\n",index);
			if(newIndex == -1)
				break;

			formatParagraph(text.substring(index,newIndex),
				maxLineLength,tabSize,buf);
			buf.append("\n\n");
			index = newIndex + 2;
		}

		if(index != text.length())
		{
			formatParagraph(text.substring(index),
				maxLineLength,tabSize,buf);
		}

		return buf.toString();
	} 

	
	
	public static int indexIgnoringWhitespace(String str, int index)
	{
		int j = 0;
		for(int i = 0; i < index; i++)
			if(!Character.isWhitespace(str.charAt(i))) j++;
		return j;
	} 

	
	
	public static int ignoringWhitespaceIndex(String str, int index)
	{
		int j = 0;
		for(int i = 0;;i++)
		{
			if(!Character.isWhitespace(str.charAt(i))) j++;

			if(j > index)
				return i;
			if(i == str.length() - 1)
				return i + 1;
		}
	} 

	
	public static final int MIXED = 0;
	public static final int LOWER_CASE = 1;
	public static final int UPPER_CASE = 2;
	public static final int TITLE_CASE = 3;

	
	public static int getStringCase(String str)
	{
		if(str.length() == 0)
			return MIXED;

		int state = -1;

		char ch = str.charAt(0);
		if(Character.isLetter(ch))
		{
			if(Character.isUpperCase(ch))
				state = UPPER_CASE;
			else
				state = LOWER_CASE;
		}

		for(int i = 1; i < str.length(); i++)
		{
			ch = str.charAt(i);
			if(!Character.isLetter(ch))
				continue;

			switch(state)
			{
			case UPPER_CASE:
				if(Character.isLowerCase(ch))
				{
					if(i == 1)
						state = TITLE_CASE;
					else
						return MIXED;
				}
				break;
			case LOWER_CASE:
			case TITLE_CASE:
				if(Character.isUpperCase(ch))
					return MIXED;
				break;
			}
		}

		return state;
	} 

	
	
	public static String toTitleCase(String str)
	{
		if(str.length() == 0)
			return str;
		else
		{
			return Character.toUpperCase(str.charAt(0))
				+ str.substring(1).toLowerCase();
		}
	} 

	
	private static final int WHITESPACE = 0;
	private static final int WORD_CHAR = 1;
	private static final int SYMBOL = 2;

	
	private static void formatParagraph(String text, int maxLineLength,
		int tabSize, StringBuilder buf)
	{
		
		int leadingWhitespaceCount = StandardUtilities.getLeadingWhiteSpace(text);
		String leadingWhitespace = text.substring(0,leadingWhitespaceCount);
		int leadingWhitespaceWidth = StandardUtilities.getLeadingWhiteSpaceWidth(text,tabSize);

		buf.append(leadingWhitespace);

		int lineLength = leadingWhitespaceWidth;
		StringTokenizer st = new StringTokenizer(text);
		while(st.hasMoreTokens())
		{
			String word = st.nextToken();
			if(lineLength == leadingWhitespaceWidth)
			{
				
			}
			else if(lineLength + word.length() + 1 > maxLineLength)
			{
				buf.append('\n');
				buf.append(leadingWhitespace);
				lineLength = leadingWhitespaceWidth;
			}
			else
			{
				buf.append(' ');
				lineLength++;
			}
			buf.append(word);
			lineLength += word.length();
		}
	} 

	
	public static void indexIgnoringWhitespace(String text, int maxLineLength,
		int tabSize, StringBuffer buf)
	{
		
		int leadingWhitespaceCount = StandardUtilities.getLeadingWhiteSpace(text);
		String leadingWhitespace = text.substring(0,leadingWhitespaceCount);
		int leadingWhitespaceWidth = StandardUtilities.getLeadingWhiteSpaceWidth(text,tabSize);

		buf.append(leadingWhitespace);

		int lineLength = leadingWhitespaceWidth;
		StringTokenizer st = new StringTokenizer(text);
		while(st.hasMoreTokens())
		{
			String word = st.nextToken();
			if(lineLength == leadingWhitespaceWidth)
			{
				
			}
			else if(lineLength + word.length() + 1 > maxLineLength)
			{
				buf.append('\n');
				buf.append(leadingWhitespace);
				lineLength = leadingWhitespaceWidth;
			}
			else
			{
				buf.append(' ');
				lineLength++;
			}
			buf.append(word);
			lineLength += word.length();
		}
	} 

	
}
