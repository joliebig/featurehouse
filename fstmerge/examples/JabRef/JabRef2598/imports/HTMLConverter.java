package net.sf.jabref.imports; 

import net.sf.jabref.export.layout.LayoutFormatter; 


public  class  HTMLConverter implements  LayoutFormatter {
	

    public String format(String text) {

        if (text == null)
            return null;
        text = text.replaceAll("&ldquo;", "``");
        text = text.replaceAll("&rdquo;", "''");
        text = text.replaceAll("&lsquo;", "`");
        text = text.replaceAll("&rsquo;", "'");
        StringBuffer sb = new StringBuffer();
        for (int i=0; i<text.length(); i++) {

            int c = text.charAt(i);

            if (c == '&') {
                i = readHtmlChar(text, sb, i);
                
            } else if (c == '<') {
                i = readTag(text, sb, i);
            } else
                sb.append((char)c);

        }

        return sb.toString();
    }


	

    private final int MAX_TAG_LENGTH = 20;

	
    private final int MAX_CHAR_LENGTH = 10;

	

    private int readHtmlChar(String text, StringBuffer sb, int position) {
        
        int index = text.indexOf(';', position);
        if ((index > position) && (index-position < MAX_CHAR_LENGTH)) {
        	
            
            return index; 
        } else return position; 
    }


	

    private int readTag(String text, StringBuffer sb, int position) {
        
        int index = text.indexOf('>', position);
        if ((index > position) && (index-position < MAX_TAG_LENGTH)) {
            
            return index; 
        } else return position; 
    }



}
