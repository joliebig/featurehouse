
package com.lowagie.text.rtf.parser.destinations;

import java.awt.Color;
import java.util.HashMap;

import com.lowagie.text.rtf.parser.RtfImportMgr;
import com.lowagie.text.rtf.parser.RtfParser;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordData;
import com.lowagie.text.rtf.parser.enumerations.RtfColorThemes;


public class RtfDestinationColorTable extends RtfDestination  {

    
    private RtfImportMgr importHeader = null;
    
    private int colorNr = 0;
    
    private int red = -1;
    
    private int green = -1;
    
    private int blue = -1;
    
    
    private int ctint = 255;
    
    private int cshade = 255;
    
    private int themeColor = RtfColorThemes.THEME_UNDEFINED;
    
    private HashMap colorMap = null;
    
    
    public RtfDestinationColorTable() {
        super(null);
        colorMap = new HashMap();
        this.colorNr = 0;
    }
    
    
    public RtfDestinationColorTable(RtfParser parser) {
        super(parser);
        colorMap = new HashMap();
        this.colorNr = 0;
        this.importHeader = parser.getImportManager();
        this.setToDefaults();
    }
    
    public void setParser(RtfParser parser) {
        this.rtfParser = parser;
        colorMap = new HashMap();
        this.colorNr = 0;
        this.importHeader = parser.getImportManager();
        this.setToDefaults();
    }
    
    public boolean handleOpeningSubGroup() {
        return true;
    }

    public boolean closeDestination() {
        return true;
    }

    public boolean handleCloseGroup() {
        processColor();
        return true;
    }
    public boolean handleOpenGroup() {
        return true;
    }
    
    public boolean handleCharacter(int ch) {
        
        if(ch == ';') {
            this.processColor();
        }
        return true;
    }
    
    public boolean handleControlWord(RtfCtrlWordData ctrlWordData) {
        if(ctrlWordData.ctrlWord.equals("blue")) this.setBlue(ctrlWordData.intValue());
        if(ctrlWordData.ctrlWord.equals("red")) this.setRed(ctrlWordData.intValue());
        if(ctrlWordData.ctrlWord.equals("green")) this.setGreen(ctrlWordData.intValue());
        if(ctrlWordData.ctrlWord.equals("cshade")) this.setShade(ctrlWordData.intValue());
        if(ctrlWordData.ctrlWord.equals("ctint")) this.setTint(ctrlWordData.intValue());
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    return true;
    }
    
    
    public void setToDefaults() {
        this.red = -1;
        this.green = -1;
        this.blue = -1;
        this.ctint = 255;
        this.cshade = 255;
        this.themeColor = RtfColorThemes.THEME_UNDEFINED;
        
    }
    
    private void processColor() {
        if(red != -1 && green != -1 && blue != -1) {
            if(this.rtfParser.isImport()) {
                this.importHeader.importColor(Integer.toString(this.colorNr), new Color(this.red, this.green, this.blue));
            }
        
            if(this.rtfParser.isConvert()) {
                colorMap.put(Integer.toString(this.colorNr), new Color(this.red, this.green, this.blue));
            }
        }
        this.setToDefaults();
        this.colorNr++;
    }
    
    private void setRed(int value) {
        if(value >= 0 && value <= 255) {
            this.red = value;
        }
    }
    
    private void setGreen(int value) {
        if(value >= 0 && value <= 255) {
            this.green = value;
        }
    }
    
    private void setBlue(int value) {
        if(value >= 0 && value <= 255) {
            this.blue = value;
        }
    }
    
    private void setTint(int value) {
        if(value >= 0 && value <= 255) {
            this.ctint = value;
            if(value >= 0 && value <255) {
                this.cshade = 255;
            }
        }
    }
    
    private void setShade(int value) {
        if(value >= 0 && value <= 255) {
            this.cshade = value;
            if(value >= 0 && value <255) {
                this.ctint = 255;
            }
        }
    }
    
    private void setThemeColor(int value) {
        if(value >= RtfColorThemes.THEME_UNDEFINED && value <= RtfColorThemes.THEME_MAX) {
            this.themeColor = value;
        } else {
            this.themeColor = RtfColorThemes.THEME_UNDEFINED;
        }
    }
    
    
    
    public Color getColor(String key) {
        return (Color)colorMap.get(key);
    }
    
}
