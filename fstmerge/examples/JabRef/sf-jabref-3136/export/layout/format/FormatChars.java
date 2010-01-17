package net.sf.jabref.export.layout.format;

import net.sf.jabref.Globals;
import net.sf.jabref.export.layout.LayoutFormatter;

import java.util.HashMap;


public class FormatChars implements LayoutFormatter {

    public static HashMap<String, String> CHARS = new HashMap<String, String>();

    static {
		CHARS.put("`A", "À"); 
		CHARS.put("'A", "Á"); 
		CHARS.put("^A", "Â"); 
		CHARS.put("~A", "Ã"); 
		CHARS.put("\"A", "Ä"); 
		CHARS.put("AA", "Å"); 
		CHARS.put("AE", "Æ"); 
		CHARS.put("cC", "Ç"); 
        CHARS.put("`E", "È"); 
		CHARS.put("'E", "É"); 
		CHARS.put("^E", "Ê"); 
		CHARS.put("\"E", "Ë"); 
		CHARS.put("`I", "Ì"); 
		CHARS.put("'I", "Í"); 
		CHARS.put("^I", "Î"); 
		CHARS.put("\"I", "Ï"); 
		CHARS.put("DH", "Ð"); 
		CHARS.put("~N", "Ñ"); 
		CHARS.put("`O", "Ò"); 
		CHARS.put("'O", "Ó"); 
		CHARS.put("^O", "Ô"); 
		CHARS.put("~O", "Õ"); 
		CHARS.put("\"O", "Ö"); 
		
		
		
		CHARS.put("O", "Ø"); 
		CHARS.put("`U", "Ù"); 
		CHARS.put("'U", "Ú"); 
		CHARS.put("^U", "Û"); 
		CHARS.put("\"U", "Ü"); 
		CHARS.put("'Y", "Ý"); 
		CHARS.put("TH", "Þ"); 
		CHARS.put("ss", "ß"); 
		CHARS.put("`a", "à"); 
		CHARS.put("'a", "á"); 
		CHARS.put("^a", "â"); 
		CHARS.put("~a", "ã"); 
		CHARS.put("\"a", "ä"); 
		CHARS.put("aa", "å"); 
		CHARS.put("ae", "æ"); 
		CHARS.put("cc", "ç"); 
		CHARS.put("`e", "è"); 
		CHARS.put("'e", "é"); 
		CHARS.put("^e", "ê"); 
		CHARS.put("\"e", "ë"); 
		CHARS.put("`i", "ì"); 
		CHARS.put("'i", "í"); 
		CHARS.put("^i", "î"); 
		CHARS.put("\"i", "ï"); 
		CHARS.put("dh", "ð"); 
		CHARS.put("~n", "ñ"); 
		CHARS.put("`o", "ò"); 
		CHARS.put("'o", "ó"); 
		CHARS.put("^o", "ô"); 
		CHARS.put("~o", "õ"); 
		CHARS.put("\"o", "ö"); 
		
		
		
		CHARS.put("o", "ø"); 
		CHARS.put("`u", "ù"); 
		CHARS.put("'u", "ú"); 
		CHARS.put("^u", "û"); 
		CHARS.put("\"u", "ü"); 
		CHARS.put("'y", "ý"); 
		CHARS.put("th", "þ"); 
		CHARS.put("\"y", "ÿ"); 

		
		
		CHARS.put("=A", "Ā"); 
		CHARS.put("=a", "ā"); 
		CHARS.put("uA", "Ă"); 
		CHARS.put("ua", "ă"); 
		CHARS.put("kA", "Ą"); 
		CHARS.put("ka", "ą"); 
		CHARS.put("'C", "Ć"); 
		CHARS.put("'c", "ć"); 
		CHARS.put("^C", "Ĉ"); 
		CHARS.put("^c", "ĉ"); 
		CHARS.put(".C", "Ċ"); 
		CHARS.put(".c", "ċ"); 
		CHARS.put("vC", "Č"); 
		CHARS.put("vc", "č"); 
		CHARS.put("vD", "Ď"); 
		
		CHARS.put("DJ", "Đ"); 
		CHARS.put("dj", "đ"); 
		CHARS.put("=E", "Ē"); 
		CHARS.put("=e", "ē"); 
		CHARS.put("uE", "Ĕ"); 
		CHARS.put("ue", "ĕ"); 
		CHARS.put(".E", "Ė"); 
		CHARS.put(".e", "ė"); 
		CHARS.put("kE", "Ę"); 
		CHARS.put("ke", "ę"); 
		CHARS.put("vE", "Ě"); 
		CHARS.put("ve", "ě"); 
		CHARS.put("^G", "Ĝ"); 
		CHARS.put("^g", "ĝ"); 
		CHARS.put("uG", "Ğ"); 
		CHARS.put("ug", "ğ"); 
		CHARS.put(".G", "Ġ"); 
		CHARS.put(".g", "ġ"); 
		CHARS.put("cG", "Ģ"); 
		CHARS.put("'g", "ģ"); 
		CHARS.put("^H", "Ĥ"); 
		CHARS.put("^h", "ĥ"); 
		CHARS.put("Hstrok", "Ħ"); 
		CHARS.put("hstrok", "ħ"); 
		CHARS.put("~I", "Ĩ"); 
		CHARS.put("~i", "ĩ"); 
		CHARS.put("=I", "Ī"); 
		CHARS.put("=i", "ī"); 
		CHARS.put("uI", "Ĭ"); 
		CHARS.put("ui", "ĭ"); 
		CHARS.put("kI", "Į"); 
		CHARS.put("ki", "į"); 
		CHARS.put(".I", "İ"); 
		CHARS.put("i", "ı"); 
		
		
		CHARS.put("^J", "Ĵ"); 
		CHARS.put("^j", "ĵ"); 
		CHARS.put("cK", "Ķ"); 
		CHARS.put("ck", "ķ"); 
		
		CHARS.put("'L", "Ĺ"); 
		CHARS.put("'l", "ĺ"); 
		CHARS.put("cL", "Ļ"); 
		CHARS.put("cl", "ļ"); 
		
		
		CHARS.put("Lmidot", "Ŀ"); 
		CHARS.put("lmidot", "ŀ"); 
		CHARS.put("L", "Ł"); 
		CHARS.put("l", "ł"); 
		CHARS.put("'N", "Ń"); 
		CHARS.put("'n", "ń"); 
		CHARS.put("cN", "Ņ"); 
		CHARS.put("cn", "ņ"); 
		CHARS.put("vN", "Ň"); 
		CHARS.put("vn", "ň"); 
		
		CHARS.put("NG", "Ŋ"); 
		CHARS.put("ng", "ŋ"); 
		CHARS.put("=O", "Ō"); 
		CHARS.put("=o", "ō"); 
		CHARS.put("uO", "Ŏ"); 
		CHARS.put("uo", "ŏ"); 
		CHARS.put("HO", "Ő"); 
		CHARS.put("Ho", "ő"); 
		CHARS.put("OE", "Œ"); 
		CHARS.put("oe", "œ"); 
		CHARS.put("'R", "Ŕ"); 
		CHARS.put("'r", "ŕ"); 
		CHARS.put("cR", "Ŗ"); 
		CHARS.put("cr", "ŗ"); 
		CHARS.put("vR", "Ř"); 
		CHARS.put("vr", "ř"); 
		CHARS.put("'S", "Ś"); 
		CHARS.put("'s", "ś"); 
		CHARS.put("^S", "Ŝ"); 
		CHARS.put("^s", "ŝ"); 
		CHARS.put("cS", "Ş"); 
		CHARS.put("cs", "ş"); 
		CHARS.put("vS", "Š"); 
		CHARS.put("vs", "š"); 
		CHARS.put("cT", "Ţ"); 
		CHARS.put("ct", "ţ"); 
		CHARS.put("vT", "Ť"); 
		
		CHARS.put("Tstrok", "Ŧ"); 
		CHARS.put("tstrok", "ŧ"); 
		CHARS.put("~U", "Ũ"); 
		CHARS.put("~u", "ũ"); 
		CHARS.put("=U", "Ū"); 
		CHARS.put("=u", "ū"); 
		CHARS.put("uU", "Ŭ"); 
		CHARS.put("uu", "ŭ"); 
		CHARS.put("rU", "Ů"); 
		CHARS.put("ru", "ů"); 
		CHARS.put("HU", "ů"); 
		CHARS.put("Hu", "ű"); 
		CHARS.put("kU", "Ų"); 
		CHARS.put("ku", "ų"); 
		CHARS.put("^W", "Ŵ"); 
		CHARS.put("^w", "ŵ"); 
		CHARS.put("^Y", "Ŷ"); 
		CHARS.put("^y", "ŷ"); 
		CHARS.put("\"Y", "Ÿ"); 
		CHARS.put("'Z", "Ź"); 
		CHARS.put("'z", "ź"); 
		CHARS.put(".Z", "Ż"); 
		CHARS.put(".z", "ż"); 
		CHARS.put("vZ", "Ž"); 
		CHARS.put("vz", "ž"); 
		
        CHARS.put("%", "%"); 
    }

    public String format(String field) {
		int i;
		field = field.replaceAll("&|\\\\&", "&amp;").replaceAll("[\\n]{1,}", "<p>");

		StringBuffer sb = new StringBuffer();
		StringBuffer currentCommand = null;
		
		char c;
		boolean escaped = false, incommand = false;
		
		for (i = 0; i < field.length(); i++) {
			c = field.charAt(i);
			if (escaped && (c == '\\')) {
				sb.append('\\');
				escaped = false;
			} else if (c == '\\') {
				if (incommand){
					
					String command = currentCommand.toString();
					Object result = CHARS.get(command);
					if (result != null) {
						sb.append((String) result);
					} else {
						sb.append(command);
					}
				}
				escaped = true;
				incommand = true;
				currentCommand = new StringBuffer();
			} else if (!incommand && (c == '{' || c == '}')) {
				
			} else if (Character.isLetter(c) || (c == '%')
				|| (Globals.SPECIAL_COMMAND_CHARS.indexOf(String.valueOf(c)) >= 0)) {
				escaped = false;

                if (!incommand)
					sb.append(c);
					
				else {
					currentCommand.append(c);
                    testCharCom: if ((currentCommand.length() == 1)
						&& (Globals.SPECIAL_COMMAND_CHARS.indexOf(currentCommand.toString()) >= 0)) {
						
						
						if (i >= field.length() - 1)
							break testCharCom;

						String command = currentCommand.toString();
						i++;
						c = field.charAt(i);
						
						String combody;
						if (c == '{') {
							IntAndString part = getPart(field, i, false);
							i += part.i;
							combody = part.s;
						} else {
							combody = field.substring(i, i + 1);
							
						}
						Object result = CHARS.get(command + combody);

						if (result != null)
							sb.append((String) result);

						incommand = false;
						escaped = false;
					} else { 
						
						if (i + 1 == field.length()){
							String command = currentCommand.toString();
                            Object result = CHARS.get(command);
							
							if (result != null) {
								sb.append((String) result);
							} else {
								sb.append(command);
							}
							
						}
					}
				}
			} else {
				String argument = null;

				if (!incommand) {
					sb.append(c);
				} else if (Character.isWhitespace(c) || (c == '{') || (c == '}')) {
					
					
					

					String command = currentCommand.toString();
                                                
                    if (c == '{') {
						IntAndString part = getPart(field, i, true);
						i += part.i;
						argument = part.s;
						if (argument != null) {
							
							Object result = CHARS.get(command + argument);
							
							
							
							
							
							if (result != null) {
								sb.append((String) result);
							} else {
								sb.append(argument);
							}
						}
                    } else if (c == '}') {
                        
                        
                        
                        Object result = CHARS.get(command);
                        if (result != null) {
                            sb.append((String) result);
                        } else {
                            
                            sb.append(command);
                        }
                    } else {
						Object result = CHARS.get(command);
						if (result != null) {
							sb.append((String) result);
						} else {
							sb.append(command);
						}
						sb.append(' ');
					}
				} else {
					
				}
				
				incommand = false;
				escaped = false;
			}
		}

		return sb.toString();
	}

	private IntAndString getPart(String text, int i, boolean terminateOnEndBraceOnly) {
		char c;
		int count = 0;
		
		StringBuffer part = new StringBuffer();
		
		
		i++;
		while (i < text.length() && Character.isWhitespace(text.charAt(i))){
			i++;
		}
		
		
		while (i < text.length()){
			c = text.charAt(i);
			if (!terminateOnEndBraceOnly && count == 0 && Character.isWhitespace(c)) {
				i--; 
					 
				break;
			}
			if (c == '}' && --count < 0)
				break;
			else if (c == '{')
				count++;
			part.append(c);
			i++;
		}
		return new IntAndString(part.length(), format(part.toString()));
	}

	private class IntAndString {
		public int i;

		String s;

		public IntAndString(int i, String s) {
			this.i = i;
			this.s = s;
		}
	}
}

