package net.sf.jabref.export.layout.format;

import java.io.File;

import net.sf.jabref.Globals;
import net.sf.jabref.Util;
import net.sf.jabref.export.layout.LayoutFormatter;


public class ResolvePDF implements LayoutFormatter {

	public String format(String field) {

        
        
        String dir = Globals.prefs.get("pdfDirectory");
		File f = Util.expandFilename(field, new String[] { dir, "." });
		
		
		if (f != null) {
			return f.toURI().toString();
		} else {
			return field;
		}
	}
}
