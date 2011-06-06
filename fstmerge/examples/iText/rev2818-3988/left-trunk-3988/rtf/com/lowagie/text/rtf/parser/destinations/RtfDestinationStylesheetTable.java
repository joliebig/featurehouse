
 
package com.lowagie.text.rtf.parser.destinations;

import com.lowagie.text.Element;
import com.lowagie.text.rtf.parser.RtfImportMgr;
import com.lowagie.text.rtf.parser.RtfParser;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordData;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordType;
import com.lowagie.text.rtf.style.RtfParagraphStyle;
import com.lowagie.text.rtf.style.RtfStyleTypes;


public class RtfDestinationStylesheetTable extends RtfDestination {
    private String styleName = "";
    
    private RtfParagraphStyle rtfParagraphStyle = null;
    
    private String elementName = "";
    
    
    private int styleNr = 0;
    
    
    private int styleType = RtfStyleTypes.PARAGRAPH;
    
    
    
    private int alignment = Element.ALIGN_LEFT;
    
    private int justificationPercentage = 0;
    
    
    
    private int firstLineIndent = 0;
    
    private int leftIndent = 0;
    
    private int rightIndent = 0;
    
    private int adustRightIndent = 0;
    
    private int mirrorIndent = 0;
    
    
    
    private int overrideWidowControl = -1;
    
    
    
    private int AutoSpaceBetweenDBCEnglish = 0;
    
    private int AutoSpaceBetweenDBCNumbers = 0;
    
    private int noCharacterWrapping = 0;
    
    private int noWordWrapping = 0;
    
    private int noOverflowPeriodComma = 0;
    
    
    
    
    
    private RtfImportMgr importHeader = null;
    private String type = "";
    
    public RtfDestinationStylesheetTable() {
        super(null);
    }
    
    public RtfDestinationStylesheetTable(RtfParser parser, String type) {
        super(parser);
        this.importHeader = parser.getImportManager();
        this.type = type;
    }
    public void setParser(RtfParser parser) {
        this.rtfParser = parser;
        this.importHeader = parser.getImportManager();
    }
    public void setType(String value) {
        this.type = value;
    }
    public void setElementName(String value) {
        this.elementName = value;
    }
    
    public boolean handleOpeningSubGroup() {
        
        return false;
    }
    
    public boolean closeDestination() {

        return true;
    }

    public boolean handleControlWord(RtfCtrlWordData ctrlWordData) {
        boolean result = true;
        this.onCtrlWord(ctrlWordData);    
        
        if(this.rtfParser.isImport()) {
            
            
            if(ctrlWordData.ctrlWord.equals("s")) { }
            if(ctrlWordData.ctrlWord.equals("cs")) {}
            if(ctrlWordData.ctrlWord.equals("ds")) {}
            if(ctrlWordData.ctrlWord.equals("ts")) {}
            if(ctrlWordData.ctrlWord.equals("tsrowd")) {}
            
            if(ctrlWordData.ctrlWord.equals("keycode")) {}
            if(ctrlWordData.ctrlWord.equals("shift")) { }
            if(ctrlWordData.ctrlWord.equals("ctrl")) { }
            if(ctrlWordData.ctrlWord.equals("alt")) { }
            
            if(ctrlWordData.ctrlWord.equals("fn")) { }
            if(ctrlWordData.ctrlWord.equals("additive")) { }
            if(ctrlWordData.ctrlWord.equals("sbasedon")) { }
            if(ctrlWordData.ctrlWord.equals("snext")) { }
            if(ctrlWordData.ctrlWord.equals("sautoupd")) { }
            if(ctrlWordData.ctrlWord.equals("shidden")) { }
            if(ctrlWordData.ctrlWord.equals("slink")) { }
            if(ctrlWordData.ctrlWord.equals("slocked")) { }
            if(ctrlWordData.ctrlWord.equals("spersonal")) { }
            if(ctrlWordData.ctrlWord.equals("scompose")) { }
            if(ctrlWordData.ctrlWord.equals("sreply")) { }
            
            
            
            
            
            if(ctrlWordData.ctrlWord.equals("styrsid")) { }
            if(ctrlWordData.ctrlWord.equals("ssemihidden")) { }
            if(ctrlWordData.ctrlWord.equals("sqformat")) { }
            if(ctrlWordData.ctrlWord.equals("spriority")) { }
            if(ctrlWordData.ctrlWord.equals("sunhideused")) { }
            
            
            if(ctrlWordData.ctrlWord.equals("tscellwidth")) { }
            if(ctrlWordData.ctrlWord.equals("tscellwidthfts")) { }
            if(ctrlWordData.ctrlWord.equals("tscellpaddt")) { }
            if(ctrlWordData.ctrlWord.equals("tscellpaddl")) { }
            if(ctrlWordData.ctrlWord.equals("tscellpaddr")) { }
            if(ctrlWordData.ctrlWord.equals("tscellpaddb")) { }
            if(ctrlWordData.ctrlWord.equals("tscellpaddft")) { }
            if(ctrlWordData.ctrlWord.equals("tscellpaddfl")) { }
            if(ctrlWordData.ctrlWord.equals("tscellpaddfr")) { }
            if(ctrlWordData.ctrlWord.equals("tscellpaddfb")) { }
            if(ctrlWordData.ctrlWord.equals("tsvertalt")) { }
            if(ctrlWordData.ctrlWord.equals("tsvertalc")) { }
            if(ctrlWordData.ctrlWord.equals("tsvertalb")) { }
            if(ctrlWordData.ctrlWord.equals("tsnowrap")) { }
            if(ctrlWordData.ctrlWord.equals("tscellcfpat")) { }
            if(ctrlWordData.ctrlWord.equals("tscellcbpat")) { }
            if(ctrlWordData.ctrlWord.equals("tsbgbdiag")) { }
            if(ctrlWordData.ctrlWord.equals("tsbgfdiag")) { }
            if(ctrlWordData.ctrlWord.equals("tsbgcross")) { }
            if(ctrlWordData.ctrlWord.equals("tsbgdcross")) { }
            if(ctrlWordData.ctrlWord.equals("tsbgdkcross ")) { }
            if(ctrlWordData.ctrlWord.equals("tsbgdkdcross")) { }
            if(ctrlWordData.ctrlWord.equals("tsbghoriz")) { }
            if(ctrlWordData.ctrlWord.equals("tsbgvert")) { }
            if(ctrlWordData.ctrlWord.equals("tsbgdkhor")) { }
            if(ctrlWordData.ctrlWord.equals("tsbgdkvert")) { }
            if(ctrlWordData.ctrlWord.equals("tsbrdrt")) { }
            if(ctrlWordData.ctrlWord.equals("tsbrdrb")) { }
            if(ctrlWordData.ctrlWord.equals("tsbrdrl")) { }
            if(ctrlWordData.ctrlWord.equals("tsbrdrr")) { }
            if(ctrlWordData.ctrlWord.equals("tsbrdrh")) { }
            if(ctrlWordData.ctrlWord.equals("tsbrdrv")) { }
            if(ctrlWordData.ctrlWord.equals("tsbrdrdgl")) { }
            if(ctrlWordData.ctrlWord.equals("tsbrdrdgr")) { }
            if(ctrlWordData.ctrlWord.equals("tscbandsh")) { }
            if(ctrlWordData.ctrlWord.equals("tscbandsv")) { }
        }
        if(ctrlWordData.ctrlWordType == RtfCtrlWordType.FLAG || 
                ctrlWordData.ctrlWordType == RtfCtrlWordType.TOGGLE ||
                ctrlWordData.ctrlWordType == RtfCtrlWordType.VALUE) {
            this.rtfParser.getState().properties.setProperty(ctrlWordData);
        }
        
        switch(this.rtfParser.getConversionType()) {
        case RtfParser.TYPE_IMPORT_FULL:
            result = true;
            break;        
        case RtfParser.TYPE_IMPORT_FRAGMENT:
            result = true;
            break;
        case RtfParser.TYPE_CONVERT:
            result = true;
            break;
        default:    
            result = false;
            break;
        }
        return result;
    }
    
    
    public boolean handleCloseGroup() {

        return true;
    }

    
    public boolean handleOpenGroup() {

        return true;
    }
    
    public boolean handleCharacter(int ch) {
        styleName += (char)ch;
        return true;
    }
    
    public void createNewStyle() {
        
        
    }
    
    
    public int setJustificationPercentage(int percent) {
        this.justificationPercentage = percent;
        return this.justificationPercentage;
    }
    
    public int getJustificationPercentage() {
        return this.justificationPercentage;
    }
    
    public int setAlignment(int alignment) {
        this.alignment = alignment;
        return this.alignment;
    }
    
    public int getAlignment() {
        return this.alignment;
    }
    
    public int getFirstLineIndent() {
        return firstLineIndent;
    }
    
    public void setFirstLineIndent(int firstLineIndent) {
        this.firstLineIndent = firstLineIndent;
    }
    
    public int getIndent() {
        return leftIndent;
    }
    
    public void setIndent(int indent) {
        this.leftIndent = indent;
    }
    
    public int getAdustRightIndent() {
        return adustRightIndent;
    }
    
    public void setAdustRightIndent(int adustRightIndent) {
        this.adustRightIndent = adustRightIndent;
    }
    
    public int getLeftIndent() {
        return leftIndent;
    }
    
    public void setLeftIndent(int leftIndent) {
        this.leftIndent = leftIndent;
    }
    
    public int getMirrorIndent() {
        return mirrorIndent;
    }
    
    public void setMirrorIndent(int mirrorIndent) {
        this.mirrorIndent = mirrorIndent;
    }
    
    public int getRightIndent() {
        return rightIndent;
    }
    
    public void setRightIndent(int rightIndent) {
        this.rightIndent = rightIndent;
    }
    
    public int getOverrideWidowControl() {
        return overrideWidowControl;
    }
    
    public void setOverrideWidowControl(int overrideWidowControl) {
        this.overrideWidowControl = overrideWidowControl;
    }
    
    public int getAutoSpaceBetweenDBCEnglish() {
        return AutoSpaceBetweenDBCEnglish;
    }
    
    public void setAutoSpaceBetweenDBCEnglish(int autoSpaceBetweenDBCEnglish) {
        AutoSpaceBetweenDBCEnglish = autoSpaceBetweenDBCEnglish;
    }
    
    public int getAutoSpaceBetweenDBCNumbers() {
        return AutoSpaceBetweenDBCNumbers;
    }
    
    public void setAutoSpaceBetweenDBCNumbers(int autoSpaceBetweenDBCNumbers) {
        AutoSpaceBetweenDBCNumbers = autoSpaceBetweenDBCNumbers;
    }
    
    public int getNoCharacterWrapping() {
        return noCharacterWrapping;
    }
    
    public void setNoCharacterWrapping(int noCharacterWrapping) {
        this.noCharacterWrapping = noCharacterWrapping;
    }
    
    public int getNoOverflowPeriodComma() {
        return noOverflowPeriodComma;
    }
    
    public void setNoOverflowPeriodComma(int noOverflowPeriodComma) {
        this.noOverflowPeriodComma = noOverflowPeriodComma;
    }
    
    public int getNoWordWrapping() {
        return noWordWrapping;
    }
    
    public void setNoWordWrapping(int noWordWrapping) {
        this.noWordWrapping = noWordWrapping;
    }
    
    public int getStyleNr() {
        return styleNr;
    }
    
    public void setStyleNr(int styleNr) {
        this.styleNr = styleNr;
    }
    
    public int getStyleType() {
        return styleType;
    }
    
    public void setStyleType(int styleType) {
        this.styleType = styleType;
    }
    
    public void setToDefaults() {
        styleName = "";
        styleNr = 0;
        alignment = Element.ALIGN_LEFT;
        justificationPercentage = 0;
        firstLineIndent = 0;
        leftIndent = 0;
        rightIndent = 0;
        adustRightIndent = 0;
        mirrorIndent = 0;
        overrideWidowControl = -1;
        AutoSpaceBetweenDBCEnglish = 0;
        AutoSpaceBetweenDBCNumbers = 0;
        noCharacterWrapping = 0;
        noWordWrapping = 0;
        noOverflowPeriodComma = 0;
        
    }


}
