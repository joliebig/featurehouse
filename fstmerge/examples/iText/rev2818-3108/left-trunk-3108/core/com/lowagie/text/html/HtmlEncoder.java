

package com.lowagie.text.html;

import java.awt.Color;

import com.lowagie.text.Element;



public class HtmlEncoder {
    
    
    

    private static final String[] htmlCode = new String[256];
    
    static {
        for (int i = 0; i < 10; i++) {
            htmlCode[i] = "&#00" + i + ";";
        }
        
        for (int i = 10; i < 32; i++) {
            htmlCode[i] = "&#0" + i + ";";
        }
        
        for (int i = 32; i < 128; i++) {
            htmlCode[i] = String.valueOf((char)i);
        }
        
        
        htmlCode['\t'] = "\t";
        htmlCode['\n'] = "<" + HtmlTags.NEWLINE + " />\n";
        htmlCode['\"'] = "&quot;"; 
        htmlCode['&'] = "&amp;"; 
        htmlCode['<'] = "&lt;"; 
        htmlCode['>'] = "&gt;"; 
        
        for (int i = 128; i < 256; i++) {
            htmlCode[i] = "&#" + i + ";";
        }
    }
    
    
    
    

    
    private HtmlEncoder () { }
    
    
    

    
    public static String encode(String string) {
        int n = string.length();
        char character;
        StringBuffer buffer = new StringBuffer();
        
        for (int i = 0; i < n; i++) {
            character = string.charAt(i);
            
            if (character < 256) {
                buffer.append(htmlCode[character]);
            }
            else {
                
                buffer.append("&#").append((int)character).append(';');
            }
        }
        return buffer.toString();
    }
    

    
    public static String encode(Color color) {
        StringBuffer buffer = new StringBuffer("#");
        if (color.getRed() < 16) {
            buffer.append('0');
        }
        buffer.append(Integer.toString(color.getRed(), 16));
        if (color.getGreen() < 16) {
            buffer.append('0');
        }
        buffer.append(Integer.toString(color.getGreen(), 16));
        if (color.getBlue() < 16) {
            buffer.append('0');
        }
        buffer.append(Integer.toString(color.getBlue(), 16));
        return buffer.toString();
    }
    

    
    public static String getAlignment(int alignment) {
        switch(alignment) {
            case Element.ALIGN_LEFT:
                return HtmlTags.ALIGN_LEFT;
            case Element.ALIGN_CENTER:
                return HtmlTags.ALIGN_CENTER;
            case Element.ALIGN_RIGHT:
                return HtmlTags.ALIGN_RIGHT;
            case Element.ALIGN_JUSTIFIED:
            case Element.ALIGN_JUSTIFIED_ALL:
                return HtmlTags.ALIGN_JUSTIFIED;
            case Element.ALIGN_TOP:
                return HtmlTags.ALIGN_TOP;
            case Element.ALIGN_MIDDLE:
                return HtmlTags.ALIGN_MIDDLE;
            case Element.ALIGN_BOTTOM:
                return HtmlTags.ALIGN_BOTTOM;
            case Element.ALIGN_BASELINE:
                return HtmlTags.ALIGN_BASELINE;
                default:
                    return "";
        }
    }
}