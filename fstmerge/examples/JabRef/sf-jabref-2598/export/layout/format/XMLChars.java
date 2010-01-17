





















package net.sf.jabref.export.layout.format;

import java.util.Map;
import java.util.regex.Pattern;

import net.sf.jabref.Globals;
import net.sf.jabref.export.layout.LayoutFormatter;


public class XMLChars implements LayoutFormatter {
	Pattern pattern = Pattern.compile(".*\\{\\\\.*[a-zA-Z]\\}.*");

	public String format(String fieldText) {

		fieldText = firstFormat(fieldText);

		for (Map.Entry<String, String> entry : Globals.XML_CHARS.entrySet()){
			String s = entry.getKey();
			String repl = entry.getValue();
			if (repl != null)
				fieldText = fieldText.replaceAll(s, repl);
		}
		return restFormat(fieldText);
	}

	private String firstFormat(String s) {
		return s.replaceAll("&|\\\\&", "&#x0026;").replaceAll("--", "&#x2013;");
	}

	boolean[] forceReplace;
	
	private String restFormat(String toFormat) {
		
		String fieldText = toFormat.replaceAll("\\}", "").replaceAll("\\{", "");

		
		
		
		

		if (forceReplace == null){
			 forceReplace = new boolean[126];
			 for (int i = 0; i < 40; i++){
				 forceReplace[i] = true;
			 }
			 forceReplace[32] = false;
			 for (int i : new int[] { 44, 45, 63, 64, 94, 95, 96, 124 }){
				 forceReplace[i] = true;
			 }
		}
		
		StringBuffer buffer = new StringBuffer(fieldText.length() * 2);
		
		for (int i = 0; i < fieldText.length(); i++) {
			int code = (fieldText.charAt(i));
		
			
			if (code > 125 || forceReplace[code]) {
				buffer.append("&#" + code + ";");
			} else {
				buffer.append((char) code);
			}
		}
		fieldText = buffer.toString();

		
		for (Map.Entry<String, String> entry : Globals.ASCII2XML_CHARS.entrySet()){
			String s = entry.getKey();
			String repl = entry.getValue();
		
			if (repl != null)
				fieldText = fieldText.replaceAll(s, repl);
		}

		return fieldText;
	}
}
