package net.sf.jabref.export.layout.format;

import net.sf.jabref.export.layout.LayoutFormatter;


public class NoSpaceBetweenAbbreviations implements LayoutFormatter {

	
	public String format(String fieldText) {
		return fieldText.replaceAll("\\.\\s+(\\p{Lu})(?=\\.)", "\\.$1");
	}
}
