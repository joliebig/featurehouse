





















package net.sf.jabref.export.layout.format;

import net.sf.jabref.export.layout.LayoutFormatter;


public class ToLowerCase implements LayoutFormatter {

	public String format(String fieldText) {
		return fieldText.toLowerCase();
	}
}
