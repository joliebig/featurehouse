package net.sf.jabref.export.layout.format;

import net.sf.jabref.export.layout.LayoutFormatter;

/**
 * A layout formatter that is the composite of the given Formatters executed in
 * order.
 * 
 * @author $Author: apel $
 * @version $Revision: 1.1 $ ($Date: 2010-01-17 00:03:44 $)
 * 
 */
public class CompositeFormat implements LayoutFormatter {

	LayoutFormatter[] formatters;

	/**
	 * If called with this constructor, this formatter does nothing.
	 */
	public CompositeFormat() {
		// Nothing
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
