
package com.lowagie.text.rtf.parser.properties;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordData;


public class RtfProperty {
    public static final int OFF = 0;
    public static final int ON = 1;
    
    
    public static final String COLOR = "color.";
    public static final String CHARACTER = "character.";
    public static final String PARAGRAPH = "paragraph.";
    public static final String SECTION = "section.";
    public static final String DOCUMENT = "document.";
    
    
    public static final String COLOR_FG = COLOR + "fg"; 
    public static final String COLOR_BG = COLOR + "bg"; 
    
    
    public static final String CHARACTER_BOLD = CHARACTER + "bold";    
    public static final String CHARACTER_UNDERLINE = CHARACTER + "underline";
    public static final String CHARACTER_ITALIC = CHARACTER + "italic";
    public static final String CHARACTER_SIZE = CHARACTER + "size"; 
    public static final String CHARACTER_FONT = CHARACTER + "font";
    public static final String CHARACTER_STYLE = CHARACTER + "style";

    
    
    public static final int JUSTIFY_LEFT = 0;
    
    public static final int JUSTIFY_RIGHT = 1;
    
    public static final int JUSTIFY_CENTER = 2;
    
    public static final int JUSTIFY_FULL = 3;
    
    public static final String PARAGRAPH_INDENT_LEFT = PARAGRAPH + "indentLeft";    
    public static final String PARAGRAPH_INDENT_RIGHT = PARAGRAPH + "indentRight";    
    public static final String PARAGRAPH_INDENT_FIRST_LINE = PARAGRAPH + "indentFirstLine";    
    public static final String PARAGRAPH_JUSTIFICATION = PARAGRAPH + "justification";
    
    public static final String PARAGRAPH_BORDER = PARAGRAPH + "border";
    public static final String PARAGRAPH_BORDER_CELL = PARAGRAPH + "borderCell";
    
    
    public static final int PARAGRAPH_BORDER_NIL = 0;
    
    public static final int PARAGRAPH_BORDER_BOTTOM = 1;
    
    public static final int PARAGRAPH_BORDER_TOP = 2;
    
    public static final int PARAGRAPH_BORDER_LEFT = 4;
    
    public static final int PARAGRAPH_BORDER_RIGHT = 8;
    
    public static final int PARAGRAPH_BORDER_DIAGONAL_UL_LR = 16;
    
    public static final int PARAGRAPH_BORDER_DIAGONAL_UR_LL = 32;
    
    public static final int PARAGRAPH_BORDER_TABLE_HORIZONTAL = 64;
    
    public static final int PARAGRAPH_BORDER_TABLE_VERTICAL = 128;
    
    
    
    public static final int PGN_DECIMAL = 0; 
    
    public static final int PGN_ROMAN_NUMERAL_UPPERCASE = 1;
    
    public static final int PGN_ROMAN_NUMERAL_LOWERCASE = 2;
    
    public static final int PGN_LETTER_UPPERCASE = 3;
    
    public static final int PGN_LETTER_LOWERCASE = 4;
    
    public static final int SBK_NONE = 0;
    
    public static final int SBK_COLUMN = 1;
    
    public static final int SBK_EVEN = 2;
    
    public static final int SBK_ODD = 3;
    
    public static final int SBK_PAGE = 4;
    
    public static final String SECTION_NUMBER_OF_COLUMNS =  SECTION + "numberOfColumns";
    public static final String SECTION_BREAK_TYPE = SECTION + "SectionBreakType";
    public static final String SECTION_PAGE_NUMBER_POSITION_X = SECTION + "pageNumberPositionX";
    public static final String SECTION_PAGE_NUMBER_POSITION_Y = SECTION + "pageNumberPositionY";
    public static final String SECTION_PAGE_NUMBER_FORMAT = SECTION + "pageNumberFormat";
    
    
    
    public static final String PAGE_PORTRAIT = "0";
    
    public static final String PAGE_LANDSCAPE = "1";
    
    public static final String DOCUMENT_PAGE_WIDTH_TWIPS = DOCUMENT + "pageWidthTwips";
    public static final String DOCUMENT_PAGE_HEIGHT_TWIPS = DOCUMENT + "pageHeightTwips";
    public static final String DOCUMENT_MARGIN_LEFT_TWIPS = DOCUMENT + "marginLeftTwips";
    public static final String DOCUMENT_MARGIN_TOP_TWIPS = DOCUMENT + "marginTopTwips";
    public static final String DOCUMENT_MARGIN_RIGHT_TWIPS = DOCUMENT + "marginRightTwips";
    public static final String DOCUMENT_MARGIN_BOTTOM_TWIPS = DOCUMENT + "marginBottomTwips";
    public static final String DOCUMENT_PAGE_NUMBER_START = DOCUMENT + "pageNumberStart";
    public static final String DOCUMENT_ENABLE_FACING_PAGES = DOCUMENT + "enableFacingPages";
    public static final String DOCUMENT_PAGE_ORIENTATION = DOCUMENT + "pageOrientation";
    public static final String DOCUMENT_DEFAULT_FONT_NUMER = DOCUMENT + "defaultFontNumber";
    
    
    protected HashMap properties = new HashMap();
    
    private boolean modifiedCharacter = false; 
    private boolean modifiedParagraph = false; 
    private boolean modifiedSection = false; 
    private boolean modifiedDocument = false; 

    
    
    private ArrayList listeners = new ArrayList();
    
    public void setToDefault() {
        setToDefault(COLOR);
        setToDefault(CHARACTER);
        setToDefault(PARAGRAPH);
        setToDefault(SECTION);
        setToDefault(DOCUMENT);
    }
    
    public void setToDefault(String propertyGroup) {
        if(COLOR.equals(propertyGroup)) {
            setProperty(COLOR_FG, new Color(0,0,0));
            setProperty(COLOR_BG, new Color(255,255,255));
            return;
        }
        if(CHARACTER.equals(propertyGroup)) {
            setProperty(CHARACTER_BOLD, 0);
            setProperty(CHARACTER_UNDERLINE, 0);
            setProperty(CHARACTER_ITALIC, 0);
            setProperty(CHARACTER_SIZE, 24);
            setProperty(CHARACTER_FONT, 0);
            return;
        }
        if(PARAGRAPH.equals(propertyGroup)) {
            setProperty(PARAGRAPH_INDENT_LEFT, 0);
            setProperty(PARAGRAPH_INDENT_RIGHT, 0);
            setProperty(PARAGRAPH_INDENT_FIRST_LINE, 0);
            setProperty(PARAGRAPH_JUSTIFICATION, JUSTIFY_LEFT);
            setProperty(PARAGRAPH_BORDER, PARAGRAPH_BORDER_NIL);
            setProperty(PARAGRAPH_BORDER_CELL, PARAGRAPH_BORDER_NIL);
            return;
        }
        if(SECTION.equals(propertyGroup)) {
            setProperty(SECTION_NUMBER_OF_COLUMNS, 0);
            setProperty(SECTION_BREAK_TYPE, SBK_NONE);
            setProperty(SECTION_PAGE_NUMBER_POSITION_X, 0);
            setProperty(SECTION_PAGE_NUMBER_POSITION_Y, 0);
            setProperty(SECTION_PAGE_NUMBER_FORMAT, PGN_DECIMAL);
            return;
        }
        if(DOCUMENT.equals(propertyGroup)) {
            setProperty(DOCUMENT_PAGE_WIDTH_TWIPS, 12240);
            setProperty(DOCUMENT_PAGE_HEIGHT_TWIPS, 15480);
            setProperty(DOCUMENT_MARGIN_LEFT_TWIPS, 1800);
            setProperty(DOCUMENT_MARGIN_TOP_TWIPS, 1440);
            setProperty(DOCUMENT_MARGIN_RIGHT_TWIPS, 1800);
            setProperty(DOCUMENT_MARGIN_BOTTOM_TWIPS, 1440);
            setProperty(DOCUMENT_PAGE_NUMBER_START, 1);
            setProperty(DOCUMENT_ENABLE_FACING_PAGES, 1);
            setProperty(DOCUMENT_PAGE_ORIENTATION, PAGE_PORTRAIT);
            setProperty(DOCUMENT_DEFAULT_FONT_NUMER, 0);    
            return;
        }
    }


    
    public boolean toggleProperty(RtfCtrlWordData ctrlWordData) { 
        
        String propertyName = ctrlWordData.specialHandler;
        
        if(propertyName == null || propertyName.length() == 0) return false;
        
        Object propertyValue = getProperty(propertyName);
        if(propertyValue == null) {
            propertyValue = new Integer(RtfProperty.ON);
        } else {
            if(propertyValue instanceof Integer) {
                int value = ((Integer)propertyValue).intValue();
                if(value != 0) {
                    removeProperty(propertyName);
                }
                return true;
            } else {
                if(propertyValue instanceof Long) {
                    long value = ((Long)propertyValue).intValue();
                    if(value != 0) {
                        removeProperty(propertyName);
                    }
                    return true;
                }
            }
        }
        setProperty(propertyName, propertyValue);
        return true;
    }
    
    public boolean setProperty(RtfCtrlWordData ctrlWordData) { 
        String propertyName = ctrlWordData.specialHandler;
        Object propertyValueNew = ctrlWordData.param;
        
        
        
        setProperty(propertyName, propertyValueNew);
        return true;
    }
    
    private boolean setProperty(String propertyName, Object propertyValueNew) {
        if(propertyName == null || propertyValueNew == null) return false;
        
        Object propertyValueOld = getProperty(propertyName);
        if(propertyValueOld instanceof Integer && propertyValueNew instanceof Integer) {
            int valueOld = ((Integer)propertyValueOld).intValue();
            int valueNew = ((Integer)propertyValueNew).intValue();
            if (valueOld==valueNew) return true;
        } else {
            if(propertyValueOld instanceof Long && propertyValueNew instanceof Long) {
                long valueOld = ((Long)propertyValueOld).intValue();
                long valueNew = ((Long)propertyValueNew).intValue();
                if (valueOld==valueNew) return true;
            }
        }
        beforeChange(propertyName);
        properties.put(propertyName, propertyValueNew);
        afterChange(propertyName);
        setModified(propertyName, true);
        return true;
    }
    
    private boolean setProperty(String propertyName, int propertyValueNew) {
        if(propertyName == null) return false;
        Object propertyValueOld = getProperty(propertyName);
        if(propertyValueOld instanceof Integer) {
            int valueOld = ((Integer)propertyValueOld).intValue();
            if (valueOld==propertyValueNew) return true;
        } 
        beforeChange(propertyName);
        properties.put(propertyName, new Integer(propertyValueNew));
        afterChange(propertyName);
        setModified(propertyName, true);
        return true;
    }
    
    private boolean addToProperty(String propertyName, int propertyValue) {
        if(propertyName == null) return false;
        int value = ((Integer)properties.get(propertyName)).intValue();
        if((value | propertyValue) == value) return true;
        value |= propertyValue;
        beforeChange(propertyName);
        properties.put(propertyName, new Integer(value));
        afterChange(propertyName);
        setModified(propertyName, true);
        return true;
    }
    
    private boolean setProperty(String propertyName, long propertyValueNew) {
        if(propertyName == null) return false;
        Object propertyValueOld = getProperty(propertyName);
        if(propertyValueOld instanceof Long) {
            long valueOld = ((Long)propertyValueOld).longValue();
            if (valueOld==propertyValueNew) return true;
        } 
        beforeChange(propertyName);
        properties.put(propertyName, new Long(propertyValueNew));
        afterChange(propertyName);
        setModified(propertyName, true);
        return true;
    }
    
    private boolean addToProperty(String propertyName, long propertyValue) {
        if(propertyName == null) return false;
        long value = ((Long)properties.get(propertyName)).longValue();
        if((value | propertyValue) == value) return true;
        value |= propertyValue;
        beforeChange(propertyName);
        properties.put(propertyName, new Long(value));
        afterChange(propertyName);
        setModified(propertyName, true);
        return true;
    }
    private boolean removeProperty(String propertyName) {
        if(propertyName == null) return false;
        if(properties.containsKey(propertyName)) {
            beforeChange(propertyName);
            properties.remove(propertyName);
            afterChange(propertyName);
            setModified(propertyName, true);
        }
        return true;
    }
    
    public Object getProperty(String propertyName) {
        return properties.get(propertyName);
    }
    
    public HashMap getProperties(String propertyGroup) {
        HashMap props = new HashMap();
        if(!properties.isEmpty()) {
            
            Iterator it = properties.keySet().iterator();
            while(it.hasNext()) {
                String key = (String)it.next();
                if(key.startsWith(propertyGroup)) {
                    props.put(key, this.properties.get(key));
                }
            }
        }
        return props;
    }
    
    
    public boolean isModified() {
        return modifiedCharacter || modifiedParagraph || modifiedSection || modifiedDocument;
    }
    
    public void setModified(String propertyName, boolean modified) {
        if(propertyName.startsWith(CHARACTER)) {
            this.setModifiedCharacter(modified);
        } else {
            if(propertyName.startsWith(PARAGRAPH)) {
                this.setModifiedParagraph(modified);
            } else {
                if(propertyName.startsWith(SECTION)) {
                    this.setModifiedSection(modified);
                } else {
                    if(propertyName.startsWith(DOCUMENT)) {
                        this.setModifiedDocument(modified);
                    }
                }
            }
        }
    }
    
    public boolean isModifiedCharacter() {
        return modifiedCharacter;
    }
    
    public void setModifiedCharacter(boolean modifiedCharacter) {
        this.modifiedCharacter = modifiedCharacter;
    }
    
    public boolean isModifiedParagraph() {
        return modifiedParagraph;
    }
    
    public void setModifiedParagraph(boolean modifiedParagraph) {
        this.modifiedParagraph = modifiedParagraph;
    }
    
    public boolean isModifiedSection() {
        return modifiedSection;
    }
    
    public void setModifiedSection(boolean modifiedSection) {
        this.modifiedSection = modifiedSection;
    }
    
    public boolean isModifiedDocument() {
        return modifiedDocument;
    }
    
    public void setModifiedDocument(boolean modifiedDocument) {
        this.modifiedDocument = modifiedDocument;
    }
    
    
    public void addRtfPropertyListener(RtfPropertyListener listener) {
        listeners.add(listener);
    }
    
    public void removeRtfPropertyListener(RtfPropertyListener listener) {
        listeners.remove(listener);
    }
    
    public void beforeChange(String propertyName) {
        
        RtfPropertyListener listener;
        for (Iterator iterator = listeners.iterator(); iterator.hasNext();) {
            listener = (RtfPropertyListener) iterator.next();
            listener.beforePropertyChange(propertyName);
        }
        
        if(propertyName.startsWith(CHARACTER)) {
            
        } else {
            if(propertyName.startsWith(PARAGRAPH)) {
                
            } else {
                if(propertyName.startsWith(SECTION)) {
                    
                } else {
                    if(propertyName.startsWith(DOCUMENT)) {
                        
                    }
                }
            }
        }
    }
    
    public void afterChange(String propertyName) {
        
        RtfPropertyListener listener;
        for (Iterator iterator = listeners.iterator(); iterator.hasNext();) {
            listener = (RtfPropertyListener) iterator.next();
            listener.afterPropertyChange(propertyName);
        }

        if(propertyName.startsWith(CHARACTER)) {
            
        } else {
            if(propertyName.startsWith(PARAGRAPH)) {
                
            } else {
                if(propertyName.startsWith(SECTION)) {
                    
                } else {
                    if(propertyName.startsWith(DOCUMENT)) {
                        
                    }
                }
            }
        }
    }
}
