package net.sf.jabref.bst;


public class BibtexCaseChanger {

	String s;

	char format;

	boolean prevColon = true;

	int n;
	
	Warn warn;

	BibtexCaseChanger(String s, char format, Warn warn) {
		this.s = s;
		this.format = format;
		this.n = s.length();
		this.warn = warn;
	}

	public static String changeCase(String s, char format, Warn warn) {
		return (new BibtexCaseChanger(s, format, warn)).changeCase();
	}

	private String changeCase() {
		char[] c = s.toCharArray();

		StringBuffer sb = new StringBuffer();

		int i = 0;

		while (i < n) {
			if (c[i] == '{') {
				braceLevel++;
				if (braceLevel != 1 || i + 4 > n || c[i + 1] != '\\') {
					prevColon = false;
					sb.append(c[i]);
					i++;
					continue;
				}
				if (format == 't' && (i == 0 || (prevColon && Character.isWhitespace(c[i - 1])))) {
					sb.append(c[i]);
					i++;
					prevColon = false;
					continue;
				}
				i = convertSpecialChar(sb, c, i, format);
				continue;
			} 
			if (c[i] == '}') {
				sb.append(c[i]);
				i++;
				braceLevel = decrBraceLevel(s, braceLevel);
				prevColon = false;
				continue;
			} 
			if (braceLevel == 0) {
				i = convertChar0(c, i, sb, format);
				continue;
			} 
			sb.append(c[i]);
			i++;
		}
		checkBrace(s, braceLevel);
		return sb.toString();
	}

	int braceLevel = 0;

	int decrBraceLevel(String string, int braceLevel) {
		if (braceLevel == 0) {
			complain(string);
		} else {
			braceLevel--;
		}
		return braceLevel;
	}

	static void complain(String s) {
		System.out.println("Warning -- String is not brace-balanced: " + s);
	}

	static void checkBrace(String s, int braceLevel) {
		if (braceLevel > 0) {
			complain(s);
		}
	}

	
	public int convertSpecialChar(StringBuffer sb, char[] c, int i, char format) {

		sb.append(c[i]); i++; 

		while (i < c.length && braceLevel > 0) {
			sb.append(c[i]); i++;
			

			String s = findSpecialChar(c, i);
			if (s != null) {
				i = convertAccented(c, i, s, sb, format);
			}

			while (i < c.length && braceLevel > 0 && c[i] != '\\') {
				if (c[i] == '}')
					braceLevel--;
				else if (c[i] == '{') {
					braceLevel++;
				}
				i = convertNonControl(c, i, sb, format);
			}
		}
		return i;
	}

	
	int convertAccented(char[] c, int pos, String s, StringBuffer sb, char format) {
		pos += s.length();

		switch (format) {
		case TITLE_LOWERS:
		case ALL_LOWERS:
			if ("L O OE AE AA".indexOf(s) != -1)
				sb.append(s.toLowerCase());
			else
				sb.append(s);
			break;
		case ALL_UPPERS:
			if ("l o oe ae aa".indexOf(s) != -1)
				sb.append(s.toUpperCase());
			else if ("i j ss".indexOf(s) != -1) {

				sb.deleteCharAt(sb.length() - 1); 
				sb.append(s.toUpperCase());
				while (pos < c.length && Character.isWhitespace(c[pos])) {
					pos++;
				}
			} else {
				sb.append(s);
			}
			break;
		}
		return pos;
	}

	int convertNonControl(char[] c, int pos, StringBuffer sb, char format) {
		switch (format) {
		case TITLE_LOWERS:
		case ALL_LOWERS:
			sb.append(Character.toLowerCase(c[pos]));
			pos++;
			break;
		case ALL_UPPERS:
			sb.append(Character.toUpperCase(c[pos]));
			pos++;
			break;
		}
		return pos;
	}

	public final static char TITLE_LOWERS = 't';

	public final static char ALL_LOWERS = 'l';

	public final static char ALL_UPPERS = 'u';

	int convertChar0(char[] c, int i, StringBuffer sb, char format) {
		switch (format) {
		case TITLE_LOWERS:
			if (i == 0) {
				sb.append(c[i]);
			} else if (prevColon && Character.isWhitespace(c[i - 1])) {
				sb.append(c[i]);
			} else {
				sb.append(Character.toLowerCase(c[i]));
			}
			if (c[i] == ':')
				prevColon = true;
			else if (!Character.isWhitespace(c[i]))
				prevColon = false;
			break;
		case ALL_LOWERS:
			sb.append(Character.toLowerCase(c[i]));
			break;
		case ALL_UPPERS:
			sb.append(Character.toUpperCase(c[i]));
		}
		i++;
		return i;
	}

	static String findSpecialChar(char[] c, int pos) {
		if (pos + 1 < c.length) {
			if (c[pos] == 'o' && c[pos + 1] == 'e')
				return "oe";
			if (c[pos] == 'O' && c[pos + 1] == 'E')
				return "OE";
			if (c[pos] == 'a' && c[pos + 1] == 'e')
				return "ae";
			if (c[pos] == 'A' && c[pos + 1] == 'E')
				return "AE";
			if (c[pos] == 's' && c[pos + 1] == 's')
				return "ss";
			if (c[pos] == 'A' && c[pos + 1] == 'A')
				return "AA";
			if (c[pos] == 'a' && c[pos + 1] == 'a')
				return "aa";
		}
		if (c[pos] == 'i')
			return String.valueOf(c[pos]);
		if (c[pos] == 'j')
			return String.valueOf(c[pos]);
		if (c[pos] == 'o')
			return String.valueOf(c[pos]);
		if (c[pos] == 'O')
			return String.valueOf(c[pos]);
		if (c[pos] == 'l')
			return String.valueOf(c[pos]);
		if (c[pos] == 'L')
			return String.valueOf(c[pos]);
		return null;
	}
}
