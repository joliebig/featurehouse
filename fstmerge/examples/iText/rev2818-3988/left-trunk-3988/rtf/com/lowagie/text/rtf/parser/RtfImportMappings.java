
package com.lowagie.text.rtf.parser;

import java.awt.Color;
import java.util.HashMap;


public class RtfImportMappings {
    
    private HashMap fontMappings = null;
    
    private HashMap colorMappings = null;
    
    private HashMap listMappings = null;
    
    private HashMap stylesheetListMappings = null;
    
    
    public RtfImportMappings() {
        this.fontMappings = new HashMap();
        this.colorMappings = new HashMap();
        this.listMappings = new HashMap();
        this.stylesheetListMappings = new HashMap();
    }
    
    
    public void addFont(String fontNr, String fontName) {
        this.fontMappings.put(fontNr, fontName);
    }
    
    public void addColor(String colorNr, Color color) {
        this.colorMappings.put(colorNr, color);
    }
    
    public void addList(String listNr, String list) {
        this.listMappings.put(listNr, list);
    }
    
    public void addStylesheetList(String stylesheetListNr, String list) {
        this.stylesheetListMappings.put(stylesheetListNr, list);
    }    
    
    
    public HashMap getFontMappings() {
        return this.fontMappings;
    }
    
    
    public HashMap getColorMappings() {
        return this.colorMappings;
    }    
    
    
    public HashMap getListMappings() {
        return this.listMappings;
    }    
    
    
    public HashMap getStylesheetListMappings() {
        return this.stylesheetListMappings;
    }
}
