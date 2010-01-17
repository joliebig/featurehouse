package net.sf.jabref.export.layout.format;

import net.sf.jabref.export.layout.LayoutFormatter;


public class LastPage implements LayoutFormatter {

    public String format(String s) {
        if (s == null)
			return "";
		String[] pageParts = s.split("[\\-]+");
		if (pageParts.length == 2)
            return pageParts[1];
        else return "";

    }
}