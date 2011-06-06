
package com.lowagie.text.rtf.direct;

import java.awt.Color;
import java.util.HashMap;


public class RtfImportMappings {
    
    private HashMap<String, String> fontMappings = null;
    
    private HashMap<String, Color> colorMappings = null;
    
    
    public RtfImportMappings() {
        this.fontMappings = new HashMap<String, String>();
        this.colorMappings = new HashMap<String, Color>();
    }
    
    
    public void addFont(String fontNr, String fontName) {
        this.fontMappings.put(fontNr, fontName);
    }
    
    
    public void addColor(String colorNr, Color color) {
        this.colorMappings.put(colorNr, color);
    }
    
    
    public HashMap<String, String> getFontMappings() {
        return this.fontMappings;
    }
    
    
    public HashMap<String, Color> getColorMappings() {
        return this.colorMappings;
    }
}
