
package com.lowagie.text.rtf.parser;


import java.awt.Color;
import java.util.HashMap;

import com.lowagie.text.Document;
import com.lowagie.text.List;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.list.RtfList;
import com.lowagie.text.rtf.style.RtfColor;
import com.lowagie.text.rtf.style.RtfFont;


public class RtfImportMgr {
    
    
    private HashMap importFontMapping = null;
    
    private HashMap importColorMapping = null;
    
    private HashMap importStylesheetListMapping = null;
    
    private HashMap importListMapping = null;
    
    private RtfDocument rtfDoc = null;
    
    private Document doc = null;


    
    public RtfImportMgr(RtfDocument rtfDoc, Document doc) {
        this.rtfDoc = rtfDoc;
        this.doc = doc;
        this.importFontMapping = new HashMap();
        this.importColorMapping = new HashMap();
        this.importStylesheetListMapping = new HashMap();
        this.importListMapping = new HashMap();
    }

    
    public boolean importFont(String fontNr, String fontName) {
        RtfFont rtfFont = new RtfFont(fontName);
        if(rtfFont != null){
            rtfFont.setRtfDocument(this.rtfDoc);
            this.importFontMapping.put(fontNr, Integer.toString(this.rtfDoc.getDocumentHeader().getFontNumber(rtfFont)));
            return true;
        } else {
            return false;
        }
    }
    
    public boolean importFont(String fontNr, String fontName, int charset) {
        RtfFont rtfFont = new RtfFont(fontName);
        if(charset>= 0)
            rtfFont.setCharset(charset);
        if(rtfFont != null){
            rtfFont.setRtfDocument(this.rtfDoc);
            this.importFontMapping.put(fontNr, Integer.toString(this.rtfDoc.getDocumentHeader().getFontNumber(rtfFont)));
            return true;
        } else {
            return false;
        }
    }
    
    public boolean importFont(String fontNr, String fontName, String fontFamily, int charset) {
        RtfFont rtfFont = new RtfFont(fontName);

        if(charset>= 0)
            rtfFont.setCharset(charset);
        if(fontFamily != null && fontFamily.length() > 0)
            rtfFont.setFamily(fontFamily);
        if(rtfFont != null){
            rtfFont.setRtfDocument(this.rtfDoc);
            this.importFontMapping.put(fontNr, Integer.toString(this.rtfDoc.getDocumentHeader().getFontNumber(rtfFont)));
            return true;
        } else {
            return false;
        }
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

    
    public void importList(String listNr, List list) {
        RtfList rtfList = new RtfList(this.rtfDoc, list);

        
        
        this.importStylesheetListMapping.put(listNr, Integer.toString(this.rtfDoc.getDocumentHeader().getListNumber(rtfList)));




    }

    
    public String mapListNr(String listNr) {
        if(this.importListMapping.containsKey(listNr)) {
            return (String) this.importListMapping.get(listNr);
        } else {
            return "0";
        }
    }

    
    public boolean importStylesheetList(String listNr, List listIn) {
        RtfList rtfList = new RtfList(this.rtfDoc, listIn);

        if(rtfList != null){
            rtfList.setRtfDocument(this.rtfDoc);
            
            return true;
        } else {
            return false;
        }
    }
    
    public String mapStylesheetListNr(String listNr) {
        if(this.importStylesheetListMapping.containsKey(listNr)) {
            return (String) this.importStylesheetListMapping.get(listNr);
        } else {
            return "0";
        }
    }

}
