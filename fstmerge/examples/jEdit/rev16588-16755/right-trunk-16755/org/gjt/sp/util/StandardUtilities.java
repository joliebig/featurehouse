

package org.gjt.sp.util;



import javax.swing.text.Segment;
import java.util.Comparator;
import java.util.Stack;



public class StandardUtilities
{

	

	
	
	public static String charsToEscapes(String str)
	{
		return charsToEscapes(str,"\n\t\\\"'");
	} 

	
	
	public static String charsToEscapes(String str, String toEscape)
	{
		StringBuilder buf = new StringBuilder();
		for(int i = 0; i < str.length(); i++)
		{
			char c = str.charAt(i);
			if(toEscape.indexOf(c) != -1)
			{
				if(c == '\n')
					buf.append("\\n");
				else if(c == '\t')
					buf.append("\\t");
				else
				{
					buf.append('\\');
					buf.append(c);
				}
			}
			else
				buf.append(c);
		}
		return buf.toString();
	} 

	
	
	public static String getIndentString(String str)
	{
		StringBuilder indentString = new StringBuilder();
		for (int i = 0; i < str.length(); i++)
		{
			char ch = str.charAt(i);
			if (! Character.isWhitespace(ch))
				break;
			indentString.append(ch);
		}
		return indentString.toString();

	} 

	
	
	public static int getLeadingWhiteSpace(String str)
	{
		return getLeadingWhiteSpace((CharSequence)str);
	} 

	
	
	public static int getLeadingWhiteSpace(CharSequence str)
	{
		int whitespace = 0;
loop:		for(;whitespace < str.length();)
		{
			switch(str.charAt(whitespace))
			{
			case ' ':
			case '\t':
				whitespace++;
				break;
			default:
				break loop;
			}
		}
		return whitespace;
	} 

	
	
	public static int getTrailingWhiteSpace(String str)
	{
		int whitespace = 0;
loop:		for(int i = str.length() - 1; i >= 0; i--)
		{
			switch(str.charAt(i))
			{
				case ' ':
				case '\t':
					whitespace++;
					break;
				default:
					break loop;
			}
		}
		return whitespace;
	} 

	
	
	public static int getLeadingWhiteSpaceWidth(String str, int tabSize)
	{
		return getLeadingWhiteSpaceWidth((CharSequence)str, tabSize);
	} 

	
	
	public static int getLeadingWhiteSpaceWidth(CharSequence str, int tabSize)
	{
		int whitespace = 0;
loop:		for(int i = 0; i < str.length(); i++)
		{
			switch(str.charAt(i))
			{
				case ' ':
					whitespace++;
					break;
				case '\t':
					whitespace += tabSize -
						whitespace % tabSize;
					break;
				default:
					break loop;
			}
		}
		return whitespace;
	} 

	
	
	public static String createWhiteSpace(int len, int tabSize)
	{
		return createWhiteSpace(len,tabSize,0);
	} 

	
	public static String truncateWhiteSpace(int len, int tabSize,
		String indentStr)
	{
		StringBuilder buf = new StringBuilder();
		int indent = 0;
		for (int i = 0; indent < len && i < indentStr.length(); i++)
		{
			char c = indentStr.charAt(i);
			if (c == ' ')
			{
				indent++;
				buf.append(c);
			}
			else if (c == '\t')
			{
				int withTab = indent + tabSize - (indent % tabSize);
				if (withTab > len)
				{
					for (; indent < len; indent++)
						buf.append(' ');
				}
				else
				{
					indent = withTab;
					buf.append(c);
				}
			}
		}
		return buf.toString();
	} 
	
	
	
	public static String createWhiteSpace(int len, int tabSize, int start)
	{
		StringBuilder buf = new StringBuilder();
		if(tabSize == 0)
		{
			while(len-- > 0)
				buf.append(' ');
		}
		else if(len == 1)
			buf.append(' ');
		else
		{
			int count = (len + start % tabSize) / tabSize;
			if(count != 0)
				len += start;
			while(count-- > 0)
				buf.append('\t');
			count = len % tabSize;
			while(count-- > 0)
				buf.append(' ');
		}
		return buf.toString();
	} 

	
	
	public static int getVirtualWidth(Segment seg, int tabSize)
	{
		int virtualPosition = 0;

		for (int i = 0; i < seg.count; i++)
		{
			char ch = seg.array[seg.offset + i];

			if (ch == '\t')
			{
				virtualPosition += tabSize
					- virtualPosition % tabSize;
			}
			else
			{
				++virtualPosition;
			}
		}

		return virtualPosition;
	} 

	
	
	public static int getOffsetOfVirtualColumn(Segment seg, int tabSize,
					    int column, int[] totalVirtualWidth)
	{
		int virtualPosition = 0;

		for (int i = 0; i < seg.count; i++)
		{
			char ch = seg.array[seg.offset + i];

			if (ch == '\t')
			{
				int tabWidth = tabSize
					- virtualPosition % tabSize;
				if(virtualPosition >= column)
					return i;
				else
					virtualPosition += tabWidth;
			}
			else
			{
				if(virtualPosition >= column)
					return i;
				else
					++virtualPosition;
			}
		}

		if(totalVirtualWidth != null)
			totalVirtualWidth[0] = virtualPosition;
		return -1;
	} 

	
	
	public static int compareStrings(String str1, String str2, boolean ignoreCase)
	{
		char[] char1 = str1.toCharArray();
		char[] char2 = str2.toCharArray();

		int len = Math.min(char1.length,char2.length);

		for(int i = 0, j = 0; i < len && j < len; i++, j++)
		{
			char ch1 = char1[i];
			char ch2 = char2[j];
			if(Character.isDigit(ch1) && Character.isDigit(ch2)
				&& ch1 != '0' && ch2 != '0')
			{
				int _i = i + 1;
				int _j = j + 1;

				for(; _i < char1.length; _i++)
				{
					if(!Character.isDigit(char1[_i]))
					{
						
						break;
					}
				}

				for(; _j < char2.length; _j++)
				{
					if(!Character.isDigit(char2[_j]))
					{
						
						break;
					}
				}

				int len1 = _i - i;
				int len2 = _j - j;
				if(len1 > len2)
					return 1;
				else if(len1 < len2)
					return -1;
				else
				{
					for(int k = 0; k < len1; k++)
					{
						ch1 = char1[i + k];
						ch2 = char2[j + k];
						if(ch1 != ch2)
							return ch1 - ch2;
					}
				}

				i = _i - 1;
				j = _j - 1;
			}
			else
			{
				if(ignoreCase)
				{
					ch1 = Character.toLowerCase(ch1);
					ch2 = Character.toLowerCase(ch2);
				}

				if(ch1 != ch2)
					return ch1 - ch2;
			}
		}

		return char1.length - char2.length;
	} 

	
	
	public static class StringCompare<E> implements Comparator<E>
	{
		private boolean icase;

		public StringCompare(boolean icase)
		{
			this.icase = icase;
		}

		public StringCompare()
		{
		}

		public int compare(E obj1, E obj2)
		{
			return compareStrings(obj1.toString(),
				obj2.toString(),icase);
		}
	} 

	
	
	public static boolean objectsEqual(Object o1, Object o2)
	{
		if(o1 == null)
		{
			if(o2 == null)
				return true;
			else
				return false;
		}
		else if(o2 == null)
			return false;
		else
			return o1.equals(o2);
	} 

	
	
	public static String globToRE(String glob)
	{
		if (glob.startsWith("(re)"))
		{
			return glob.substring(4);
		}

		final Object NEG = new Object();
		final Object GROUP = new Object();
		Stack<Object> state = new Stack<Object>();

		StringBuilder buf = new StringBuilder();
		boolean backslash = false;

		for(int i = 0; i < glob.length(); i++)
		{
			char c = glob.charAt(i);
			if(backslash)
			{
				buf.append('\\');
				buf.append(c);
				backslash = false;
				continue;
			}

			switch(c)
			{
			case '\\':
				backslash = true;
				break;
			case '?':
				buf.append('.');
				break;
			case '.':
			case '+':
			case '(':
			case ')':
				buf.append('\\');
				buf.append(c);
				break;
			case '*':
				buf.append(".*");
				break;
			case '|':
				if(backslash)
					buf.append("\\|");
				else
					buf.append('|');
				break;
			case '{':
				buf.append('(');
				if(i + 1 != glob.length() && glob.charAt(i + 1) == '!')
				{
					buf.append('?');
					state.push(NEG);
				}
				else
					state.push(GROUP);
				break;
			case ',':
				if(!state.isEmpty() && state.peek() == GROUP)
					buf.append('|');
				else
					buf.append(',');
				break;
			case '}':
				if(!state.isEmpty())
				{
					buf.append(')');
					if(state.pop() == NEG)
						buf.append(".*");
				}
				else
					buf.append('}');
				break;
			default:
				buf.append(c);
			}
		}

		return buf.toString();
	} 

	
	
	public static boolean regionMatches(CharSequence seq,
					    int toff,
					    CharSequence other,
					    int ooff,
					    int len)
	{

		if (toff < 0 || ooff < 0 || len < 0)
			return false;

		boolean ret = true;
		for (int i = 0; i < len; i++)
		{
			char c1;

			if (i + toff < seq.length())
				c1 = seq.charAt(i + toff);
			else
			{
				ret = false;
				break;
			}

			char c2;
			if (i + ooff < other.length())
				c2 = other.charAt(i + ooff);
			else
			{
				ret = false;
				break;
			}

			if (c1 != c2)
			{
				ret = false;
				break;
			}
		}

		return ret;
	} 

	
	
	public static boolean startsWith(CharSequence seq, String str)
	{
		boolean ret = true;
		for (int i = 0; i < str.length(); i++)
		{
			if (i >= seq.length() ||
			    seq.charAt(i) != str.charAt(i))
			{
				ret = false;
				break;
			}
		}
		return ret;
	} 

	
	
	public static boolean getBoolean(Object obj, boolean def)
	{
		if(obj == null)
			return def;
		else if(obj instanceof Boolean)
			return ((Boolean)obj).booleanValue();
		else if("true".equals(obj) || "yes".equals(obj)
			|| "on".equals(obj))
			return true;
		else if("false".equals(obj) || "no".equals(obj)
			|| "off".equals(obj))
			return false;

		return def;
	} 

	
	private StandardUtilities(){}
}
