
package net.sf.jabref.export.layout.format;

import net.sf.jabref.export.layout.LayoutFormatter;


public class AuthorAbbreviator implements LayoutFormatter {

	
	public String format(String fieldText) {
		
		
		
		return (new AuthorLastFirstAbbreviator()).format(fieldText);
	}
}
