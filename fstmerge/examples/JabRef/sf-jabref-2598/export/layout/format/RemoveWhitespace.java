





















package net.sf.jabref.export.layout.format;

import net.sf.jabref.export.layout.LayoutFormatter;


public class RemoveWhitespace implements LayoutFormatter {

    public String format(String fieldEntry) {

        StringBuilder sb = new StringBuilder(fieldEntry.length());

        for (char c : fieldEntry.toCharArray()) {
            if (!Character.isWhitespace(c) || Character.isSpaceChar(c)) {
                sb.append(c);
            }
        }

        return sb.toString();
    }
}