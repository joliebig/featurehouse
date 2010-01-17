package net.sf.jabref.export.layout.format;

import net.sf.jabref.AuthorList;
import net.sf.jabref.export.layout.LayoutFormatter;


public class AuthorFirstFirstCommas implements LayoutFormatter {

    public String format(String fieldText) {
        return AuthorList.fixAuthor_firstNameFirstCommas(fieldText, false, false);
    }
}
