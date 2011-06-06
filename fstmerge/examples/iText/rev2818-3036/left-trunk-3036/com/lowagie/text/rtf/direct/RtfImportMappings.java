
package com.lowagie.text.rtf.direct;

import java.awt.Color;
import java.util.HashMap;


public class RtfImportMappings {
    
    private HashMap fontMappings = null;
    
    private HashMap colorMappings = null;
    
    
    public RtfImportMappings() {
        this.fontMappings = new HashMap();
        this.colorMappings = new HashMap();
    }
    
    
    public void addFont(String fontNr, String fontName) {
        this.fontMappings.put(fontNr, fontName);
    }
    
    
    public void addColor(String colorNr, Color color) {
        this.colorMappings.put(colorNr, color);
    }
    
    
    public HashMap getFontMappings() {
        return this.fontMappings;
    }
    
    
    public HashMap getColorMappings() {
        return this.colorMappings;
    }
}
