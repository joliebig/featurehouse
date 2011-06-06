
package com.lowagie.text.rtf.direct;

import java.awt.Color;


public class RtfColorTableParser {
    
    private RtfImportHeader importHeader = null;
    
    private int colorNr = 0;
    
    private int red = -1;
    
    private int green = -1;
    
    private int blue = -1;
    
    
    public RtfColorTableParser(RtfImportHeader importHeader) {
        this.importHeader = importHeader;
        this.colorNr = 0;
        this.red = -1;
        this.green = -1;
        this.blue = -1;
    }

    public static boolean stringMatches(String text, String start) {
        if (!text.startsWith(start))
            return false;
        int first = start.length();
        int last = text.length();
        if (first == last)
            return false;
        for (int k = first; k < last; ++k) {
            char c = text.charAt(k);
            if (c < '0' || c > '9')
                return false;
        }
        return true;
    }
    
    
    public void handleCtrlWord(String ctrlWord, int groupLevel) {
        if (stringMatches(ctrlWord, "\\red"))
            this.red = Integer.parseInt(ctrlWord.substring(4));
        else if (stringMatches(ctrlWord, "\\green"))
            this.green = Integer.parseInt(ctrlWord.substring(6));
        else if (stringMatches(ctrlWord, "\\blue"))
            this.blue = Integer.parseInt(ctrlWord.substring(5));
    }
    
    
    public void handleText(String text, int groupLevel) {
        if(text.indexOf(';') != -1) {
            if(red != -1 && green != -1 && blue != -1) {
                this.importHeader.importColor(Integer.toString(this.colorNr), new Color(this.red, this.green, this.blue));
            }
            this.colorNr++;
        }
    }
}
