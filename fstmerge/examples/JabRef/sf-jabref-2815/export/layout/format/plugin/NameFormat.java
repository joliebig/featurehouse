package net.sf.jabref.export.layout.format.plugin;

import net.sf.jabref.AuthorList;
import net.sf.jabref.BibtexEntry;
import net.sf.jabref.bst.BibtexNameFormatter;
import net.sf.jabref.export.layout.LayoutFormatter;


public class NameFormat implements LayoutFormatter {

	public static final String DEFAULT_FORMAT = "1@*@{ff }{vv }{ll}{, jj}@@*@1@{ff }{vv }{ll}{, jj}@*@, {ff }{vv }{ll}{, jj}";

	public String format(String toFormat, AuthorList al, String[] formats){
		
		StringBuffer sb = new StringBuffer();
		
		int n = al.size();
		
		for (int i = 1; i <= al.size(); i++){
			for (int j = 1; j < formats.length; j+=2){
				if (formats[j].equals("*")){
					sb.append(BibtexNameFormatter.formatName(toFormat, i, formats[j+1], null));
					break;
				} else {
					String[] range = formats[j].split("\\.\\.");
					
					int s,e;
					if (range.length == 2){
						s = Integer.parseInt(range[0]);
						e = Integer.parseInt(range[1]);
					} else {
						s = e = Integer.parseInt(range[0]); 
					}
					if (s < 0) s += n + 1;
					if (e < 0) e += n + 1;
					if (e < s) { int temp = e; e = s; s = temp; }

					if (s <= i && i <= e){
						sb.append(BibtexNameFormatter.formatName(toFormat, i, formats[j+1], null));
						break;
					}
				}
			}
		}	
		return sb.toString();
		
	}
	
	public String format(String toFormat, String parameters, BibtexEntry currentEntry) {
		
		AuthorList al = AuthorList.getAuthorList(toFormat);
		
		if (parameters == null || parameters.length() == 0){
			parameters = "*:*:\"{ff}{vv}{ll}{,jj} \"";
		}
		
		String[] cases = parameters.split("@@");
		for (int i = 0; i < cases.length; i++){
			String[] formatString = cases[i].split("@");
			
			if (formatString.length < 3){
				
				return toFormat;
			}
			
			if (formatString[0].equals("*")){
				return format(toFormat, al, formatString);
			} else {
				if (al.size() <= Integer.parseInt(formatString[0])){
					return format(toFormat, al, formatString);
				}
			}
		}
		return toFormat;
	}

	public String format(String fieldText) {
		return format(fieldText, parameter, null);
	}

	String parameter = DEFAULT_FORMAT;
	
	public void setParameter(String parameter) {
		this.parameter = parameter;
	}
}
