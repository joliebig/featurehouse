package net.sf.jabref.export.layout.format;

import net.sf.jabref.Globals;
import net.sf.jabref.export.layout.LayoutFormatter;


public class HTMLChars implements LayoutFormatter {

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
					Object result = Globals.HTMLCHARS.get(command);
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
						Object result = Globals.HTMLCHARS.get(command + combody);

						if (result != null)
							sb.append((String) result);

						incommand = false;
						escaped = false;
					} else { 
						
						if (i + 1 == field.length()){
							String command = currentCommand.toString();
                            Object result = Globals.HTMLCHARS.get(command);
							
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
                                                
                    
					
					
					if (command.equals("em") || command.equals("emph") || command.equals("textit")) {
						IntAndString part = getPart(field, i, true);

						i += part.i;
						sb.append("<em>").append(part.s).append("</em>");
					} else if (command.equals("textbf")) {
						IntAndString part = getPart(field, i, true);
						i += part.i;
						sb.append("<b>").append(part.s).append("</b>");
					} else if (c == '{') {
						IntAndString part = getPart(field, i, true);
						i += part.i;
						argument = part.s;
						if (argument != null) {
							
							Object result = Globals.HTMLCHARS.get(command + argument);
							
							
							
							
							
							if (result != null) {
								sb.append((String) result);
							} else {
								sb.append(argument);
							}
						}
                    } else if (c == '}') {
                        
                        
                        
                        Object result = Globals.HTMLCHARS.get(command);
                        if (result != null) {
                            sb.append((String) result);
                        } else {
                            
                            sb.append(command);
                        }
                    } else {
						Object result = Globals.HTMLCHARS.get(command);
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
