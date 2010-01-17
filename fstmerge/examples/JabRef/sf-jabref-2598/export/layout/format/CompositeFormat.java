package net.sf.jabref.export.layout.format;

import net.sf.jabref.export.layout.LayoutFormatter;


public class CompositeFormat implements LayoutFormatter {

	LayoutFormatter[] formatters;

	
	public CompositeFormat() {
		
	}

	public CompositeFormat(LayoutFormatter first, LayoutFormatter second) {
		formatters = new LayoutFormatter[] { first, second };
	}

	public CompositeFormat(LayoutFormatter[] formatters) {
		this.formatters = formatters;
	}

	public String format(String fieldText) {
		if (formatters != null) {
			for (int i = 0; i < formatters.length; i++) {
				fieldText = formatters[i].format(fieldText);
			}
		}
		return fieldText;
	}

}
