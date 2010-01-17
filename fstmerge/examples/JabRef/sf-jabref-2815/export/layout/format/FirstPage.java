package net.sf.jabref.export.layout.format;

import net.sf.jabref.export.layout.LayoutFormatter;


public class FirstPage implements LayoutFormatter {

    public String format(String s) {
        if (s == null)
			return "";
		String[] pageParts = s.split("[\\-]+");
		if (pageParts.length == 2)
            return pageParts[0];
        else return "";

    }
}
