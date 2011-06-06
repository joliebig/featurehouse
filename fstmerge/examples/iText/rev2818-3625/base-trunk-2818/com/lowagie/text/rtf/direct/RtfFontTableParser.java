
package com.lowagie.text.rtf.direct;


public class RtfFontTableParser {
    
    private RtfImportHeader importHeader = null;
    
    private String fontNr = "";
    
    private String fontName = "";
    
    
    public RtfFontTableParser(RtfImportHeader importHeader) {
        this.importHeader = importHeader;
        this.fontNr = "";
        this.fontName = "";
    }

    
    public void handleCloseGroup(int groupLevel) {
        if(groupLevel == 3 && !this.fontNr.equals("") && !this.fontName.equals("")) {
            this.importHeader.importFont(this.fontNr, this.fontName);
            this.fontNr = "";
            this.fontName = "";
        }
    }

    
    public void handleCtrlWord(String ctrlWord, int groupLevel) {
        if(RtfColorTableParser.stringMatches(ctrlWord, "\\f") && groupLevel == 3) {
            this.fontNr = ctrlWord.substring(2);
        }
    }
    
    
    public void handleText(String text, int groupLevel) {
        if(text.indexOf(';') >= 0 && groupLevel == 3) {
            this.fontName = text.substring(0, text.indexOf(';'));
        }
    }
}
