package net.sf.jabref.export.layout.format;

import net.sf.jabref.AuthorList;
import net.sf.jabref.export.layout.LayoutFormatter;


public class AuthorLastFirstAbbreviator implements LayoutFormatter {

	
	public String format(String fieldText) {
		
		
		
		try {
		  return getAbbreviations(fieldText.split(" and "));
		} catch(Exception e){
            return fieldText;
		}
	}

	
	private String getAbbreviations(String[] authors) {
		if (authors.length == 0)
			return "";

		if (!isProperFormat(authors)) {
			throw new IllegalArgumentException("Author names must be formatted \"Last, First\" or \"Last, Jr., First\" before formatting with AuthorLastFirstAbbreviator");
		}

		for (int i = 0; i < authors.length; i++) {
			authors[i] = getAbbreviation(authors[i]);
		}

		StringBuffer sb = new StringBuffer();

		for (int i = 0; i < authors.length - 1; i++) {
			sb.append(authors[i]).append(" and ");
		}
		sb.append(authors[authors.length - 1]);

		return sb.toString();
	}

	
	private boolean isProperFormat(String[] authors) {
        for (int i = 0; i < authors.length; i++) {
			if ((authors[i].indexOf(' ') >= 0)
					&& (authors[i].indexOf(',') == -1)) {
				return false;
			}

		}
		return true;
	}

	
	private String getAbbreviation(String author) {

		String[] parts = author.split(",");

		String last, first;

		switch (parts.length) {
		case 1:
			
			return author;
		case 2:
			last = parts[0].trim();
			first = parts[1].trim();
			break;
		case 3:
			last = parts[0].trim();
			
			first = parts[2].trim();
			break;
		default:
			throw new IllegalArgumentException("Authorname contained 3 or more commas");
		}

		StringBuffer sb = new StringBuffer();
		sb.append(last);
		sb.append(", ");

		String[] firstNames = first.split(" ");
		for (int i = 0; i < firstNames.length; i++) {
			sb.append(firstNames[i].charAt(0));
			sb.append('.');
			if (i < firstNames.length - 1)
				sb.append(' ');
		}
		return sb.toString();
	}
}
