
package com.lowagie.text.rtf.parser;

import java.awt.Color;
import java.util.HashMap;
import com.lowagie.text.List;


public class RtfImportMappings {
    
    private HashMap<String, String> fontMappings = null;
    
    private HashMap<String, Color> colorMappings = null;
    
    private HashMap<String, String> listMappings = null;
    
    private HashMap<String, List> stylesheetListMappings = null;
    
    
    public RtfImportMappings() {
        this.fontMappings = new HashMap<String, String>();
        this.colorMappings = new HashMap<String, Color>();
        this.listMappings = new HashMap<String, String>();
        this.stylesheetListMappings = new HashMap<String, List>();
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
    
    public void addStylesheetList(String stylesheetListNr, List list) {
        this.stylesheetListMappings.put(stylesheetListNr, list);
    }    
    
    
    public HashMap<String, String> getFontMappings() {
        return this.fontMappings;
    }
    
    
    public HashMap<String, Color> getColorMappings() {
        return this.colorMappings;
    }    
    
    
    public HashMap<String, String> getListMappings() {
        return this.listMappings;
    }    
    
    
    public HashMap<String, List> getStylesheetListMappings() {
        return this.stylesheetListMappings;
    }
}
