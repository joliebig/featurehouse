
package com.lowagie.text.rtf.direct;

import java.awt.Color;
import java.util.HashMap;

import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.style.RtfColor;
import com.lowagie.text.rtf.style.RtfFont;


public class RtfImportHeader {
    
    private HashMap importFontMapping = null;
    
    private HashMap importColorMapping = null;
    
    private RtfDocument rtfDoc = null;
    
    
    public RtfImportHeader(RtfDocument rtfDoc) {
        this.rtfDoc = rtfDoc;
        this.importFontMapping = new HashMap();
        this.importColorMapping = new HashMap();
    }
    
    
    public void importFont(String fontNr, String fontName) {
        RtfFont rtfFont = new RtfFont(fontName);
        rtfFont.setRtfDocument(this.rtfDoc);
        this.importFontMapping.put(fontNr, Integer.toString(this.rtfDoc.getDocumentHeader().getFontNumber(rtfFont)));
    }
    
    
    public String mapFontNr(String fontNr) {
        if(this.importFontMapping.containsKey(fontNr)) {
            return (String) this.importFontMapping.get(fontNr);
        } else {
            return "0";
        }
    }
    
    
    public void importColor(String colorNr, Color color) {
        RtfColor rtfColor = new RtfColor(this.rtfDoc, color);
        this.importColorMapping.put(colorNr, Integer.toString(rtfColor.getColorNumber()));
    }
    
    
    public String mapColorNr(String colorNr) {
        if(this.importColorMapping.containsKey(colorNr)) {
            return (String) this.importColorMapping.get(colorNr);
        } else {
            return "0";
        }
    }
}
