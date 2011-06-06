
package com.lowagie.text.rtf.list;

import java.awt.Color;
import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocWriter;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.RtfExtendedElement;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.style.RtfColor;
import com.lowagie.text.rtf.style.RtfFont;
import com.lowagie.text.rtf.style.RtfFontList;
import com.lowagie.text.rtf.style.RtfParagraphStyle;
import com.lowagie.text.rtf.text.RtfParagraph;


public class RtfListLevel extends RtfElement implements RtfExtendedElement {
    
    private static final byte[] LIST_LEVEL = DocWriter.getISOBytes("\\listlevel");
    
    private static final byte[] LIST_LEVEL_TEMPLATE_ID = DocWriter.getISOBytes("\\leveltemplateid");
    
    private static final byte[] LIST_LEVEL_TYPE = DocWriter.getISOBytes("\\levelnfc");
    
    private static final byte[] LIST_LEVEL_TYPE_NEW = DocWriter.getISOBytes("\\levelnfcn");
    
    private static final byte[] LIST_LEVEL_ALIGNMENT = DocWriter.getISOBytes("\\leveljc");
    
    private static final byte[] LIST_LEVEL_ALIGNMENT_NEW = DocWriter.getISOBytes("\\leveljcn");
    
    private static final byte[] LIST_LEVEL_START_AT = DocWriter.getISOBytes("\\levelstartat");
    
    private static final byte[] LIST_LEVEL_TEXT = DocWriter.getISOBytes("\\leveltext");
    
    private static final byte[] LIST_LEVEL_STYLE_NUMBERED_BEGIN = DocWriter.getISOBytes("\\\'02\\\'");
    
    private static final byte[] LIST_LEVEL_STYLE_NUMBERED_END = DocWriter.getISOBytes(".;");
    
    private static final byte[] LIST_LEVEL_STYLE_BULLETED_BEGIN = DocWriter.getISOBytes("\\\'01");
    
    private static final byte[] LIST_LEVEL_STYLE_BULLETED_END = DocWriter.getISOBytes(";");
    
    private static final byte[] LIST_LEVEL_NUMBERS_BEGIN = DocWriter.getISOBytes("\\levelnumbers");
    
    private static final byte[] LIST_LEVEL_FOLOW = DocWriter.getISOBytes("\\levelfollow");
    
    private static final byte[] LIST_LEVEL_SPACE = DocWriter.getISOBytes("\\levelspace");
    
    private static final byte[] LIST_LEVEL_INDENT = DocWriter.getISOBytes("\\levelindent");
    
    private static final byte[] LIST_LEVEL_LEGAL = DocWriter.getISOBytes("\\levellegal");
    
    private static final byte[] LIST_LEVEL_NO_RESTART = DocWriter.getISOBytes("\\levelnorestart");
    
    private static final byte[] LIST_LEVEL_NUMBERS_NUMBERED = DocWriter.getISOBytes("\\\'01");
    
    private static final byte[] LIST_LEVEL_NUMBERS_END = DocWriter.getISOBytes(";");
    
    
    private static final byte[] LIST_LEVEL_FIRST_INDENT = DocWriter.getISOBytes("\\fi");
    
    private static final byte[] LIST_LEVEL_SYMBOL_INDENT = DocWriter.getISOBytes("\\tx");
    
    
    private static final byte[] LIST_LEVEL_TENTATIVE = DocWriter.getISOBytes("\\lvltentative");
    
    private static final byte[] LIST_LEVEL_PICTURE = DocWriter.getISOBytes("\\levelpicture");
    

    public static final int LIST_TYPE_NUMBERED = 1;
    public static final int LIST_TYPE_UPPER_LETTERS = 2;
    public static final int LIST_TYPE_LOWER_LETTERS = 3;
    public static final int LIST_TYPE_UPPER_ROMAN = 4;
    public static final int LIST_TYPE_LOWER_ROMAN = 5;

    public static final int LIST_TYPE_UNKNOWN = -1;                     
    public static final int LIST_TYPE_BASE = 1000;                         
    public static final int LIST_TYPE_ARABIC = 1000;                     
    public static final int LIST_TYPE_UPPERCASE_ROMAN_NUMERAL = 1001;    
    public static final int LIST_TYPE_LOWERCASE_ROMAN_NUMERAL = 1002;    
    public static final int LIST_TYPE_UPPERCASE_LETTER = 1003;            
    public static final int LIST_TYPE_LOWERCASE_LETTER = 1004;            
    public static final int LIST_TYPE_ORDINAL_NUMBER = 1005;            
    public static final int LIST_TYPE_CARDINAL_TEXT_NUMBER = 1006;        
    public static final int LIST_TYPE_ORDINAL_TEXT_NUMBER = 1007;        
    public static final int LIST_TYPE_ARABIC_LEADING_ZERO = 1022;        
    public static final int LIST_TYPE_BULLET = 1023;                    
    public static final int LIST_TYPE_NO_NUMBER = 1255;                

    
    private int listType = LIST_TYPE_UNKNOWN;

    
    private String bulletCharacter = "\u"; 
    
    private Chunk bulletChunk = null;
    
    private int listStartAt = 1;
    
    private int listLevel = 0;
    
    private int firstIndent = 0;
    
    private int leftIndent = 0;
    
    private int rightIndent = 0;
    
    private int symbolIndent = 0;
    
    private boolean isTentative = true;
    
    private boolean isLegal = false;
    
    
    private int listNoRestart = 0;
    public static final int LIST_LEVEL_FOLLOW_TAB = 0; 
    public static final int LIST_LEVEL_FOLLOW_SPACE = 1; 
    public static final int LIST_LEVEL_FOLLOW_NOTHING = 2; 
    private int levelFollowValue = LIST_LEVEL_FOLLOW_TAB;

    
    private int alignment = Element.ALIGN_LEFT;
    
    private int levelPicture = -1;
    
    private int levelTextNumber = 0;
    
    private RtfFont fontNumber;
    
    private RtfFont fontBullet;
    
    private int templateID = -1;
    
    private RtfListLevel listLevelParent = null;
    
    
    private RtfList parent = null;
    
    public RtfListLevel(RtfDocument doc)
    {
        super(doc);
        templateID = document.getRandomInt();
        setFontNumber( new RtfFont(document, new Font(Font.TIMES_ROMAN, 10, Font.NORMAL, new Color(0, 0, 0))));
        setBulletFont(new Font(Font.SYMBOL, 10, Font.NORMAL, new Color(0, 0, 0)));
    }
    
    public RtfListLevel(RtfDocument doc, RtfList parent)
    {
        super(doc);
        this.parent = parent;
        templateID = document.getRandomInt();
        setFontNumber( new RtfFont(document, new Font(Font.TIMES_ROMAN, 10, Font.NORMAL, new Color(0, 0, 0))));
        setBulletFont(new Font(Font.SYMBOL, 10, Font.NORMAL, new Color(0, 0, 0)));
    }
    
    public RtfListLevel(RtfListLevel ll)
    {
        super(ll.document);
        templateID = document.getRandomInt();
        this.alignment = ll.alignment;
        this.bulletCharacter = ll.bulletCharacter;
        this.firstIndent = ll.firstIndent;
        this.fontBullet = ll.fontBullet;
        this.fontNumber = ll.fontNumber;
        this.inHeader = ll.inHeader;
        this.inTable = ll.inTable;
        this.leftIndent = ll.leftIndent;
        this.listLevel = ll.listLevel;
        this.listNoRestart = ll.listNoRestart;
        this.listStartAt = ll.listStartAt;
        this.listType = ll.listType;
        this.parent = ll.parent;
        this.rightIndent = ll.rightIndent;
        this.symbolIndent = ll.symbolIndent;
    }

    
    public int getListNoRestart() {
        return listNoRestart;
    }

    
    public void setListNoRestart(int listNoRestart) {
        this.listNoRestart = listNoRestart;
    }

    
    public int getAlignment() {
        return alignment;
    }

    
    public void setAlignment(int alignment) {
        this.alignment = alignment;
    }

    public void writeDefinition(final OutputStream result) throws IOException {
        result.write(OPEN_GROUP);
        result.write(LIST_LEVEL);
        result.write(LIST_LEVEL_TYPE);
        switch(this.listType) {
            case LIST_TYPE_BULLET        : result.write(intToByteArray(23)); break;
            case LIST_TYPE_NUMBERED      : result.write(intToByteArray(0)); break;
            case LIST_TYPE_UPPER_LETTERS : result.write(intToByteArray(3)); break;
            case LIST_TYPE_LOWER_LETTERS : result.write(intToByteArray(4)); break;
            case LIST_TYPE_UPPER_ROMAN   : result.write(intToByteArray(1)); break;
            case LIST_TYPE_LOWER_ROMAN   : result.write(intToByteArray(2)); break;
            
            case LIST_TYPE_ARABIC        : result.write(intToByteArray(0)); break;
            case LIST_TYPE_UPPERCASE_ROMAN_NUMERAL        : result.write(intToByteArray(1)); break;
            case LIST_TYPE_LOWERCASE_ROMAN_NUMERAL        : result.write(intToByteArray(2)); break;
            case LIST_TYPE_UPPERCASE_LETTER        : result.write(intToByteArray(3)); break;
            case LIST_TYPE_ORDINAL_NUMBER        : result.write(intToByteArray(4)); break;
            case LIST_TYPE_CARDINAL_TEXT_NUMBER        : result.write(intToByteArray(5)); break;
            case LIST_TYPE_ORDINAL_TEXT_NUMBER        : result.write(intToByteArray(6)); break;
            case LIST_TYPE_LOWERCASE_LETTER        : result.write(intToByteArray(7)); break;
            case LIST_TYPE_ARABIC_LEADING_ZERO        : result.write(intToByteArray(22)); break;
            case LIST_TYPE_NO_NUMBER        : result.write(intToByteArray(255)); break;
            default:    
                if(this.listType >= RtfListLevel.LIST_TYPE_BASE) {
                    result.write(intToByteArray(this.listType - RtfListLevel.LIST_TYPE_BASE));
                }
            break;
        }
        
        result.write(LIST_LEVEL_TYPE_NEW);
        switch(this.listType) {
            case LIST_TYPE_BULLET        : result.write(intToByteArray(23)); break;
            case LIST_TYPE_NUMBERED      : result.write(intToByteArray(0)); break;
            case LIST_TYPE_UPPER_LETTERS : result.write(intToByteArray(3)); break;
            case LIST_TYPE_LOWER_LETTERS : result.write(intToByteArray(4)); break;
            case LIST_TYPE_UPPER_ROMAN   : result.write(intToByteArray(1)); break;
            case LIST_TYPE_LOWER_ROMAN   : result.write(intToByteArray(2)); break;
            
            case LIST_TYPE_ARABIC        : result.write(intToByteArray(0)); break;
            case LIST_TYPE_UPPERCASE_ROMAN_NUMERAL        : result.write(intToByteArray(1)); break;
            case LIST_TYPE_LOWERCASE_ROMAN_NUMERAL        : result.write(intToByteArray(2)); break;
            case LIST_TYPE_UPPERCASE_LETTER        : result.write(intToByteArray(3)); break;
            case LIST_TYPE_ORDINAL_NUMBER        : result.write(intToByteArray(4)); break;
            case LIST_TYPE_CARDINAL_TEXT_NUMBER        : result.write(intToByteArray(5)); break;
            case LIST_TYPE_ORDINAL_TEXT_NUMBER        : result.write(intToByteArray(6)); break;
            case LIST_TYPE_LOWERCASE_LETTER        : result.write(intToByteArray(7)); break;
            case LIST_TYPE_ARABIC_LEADING_ZERO        : result.write(intToByteArray(22)); break;
            case LIST_TYPE_NO_NUMBER        : result.write(intToByteArray(255)); break;
            default:    
                if(this.listType >= RtfListLevel.LIST_TYPE_BASE) {
                    result.write(intToByteArray(this.listType - RtfListLevel.LIST_TYPE_BASE));
                }
            break;
        }
        result.write(LIST_LEVEL_ALIGNMENT);
        result.write(intToByteArray(0));
        result.write(LIST_LEVEL_ALIGNMENT_NEW);
        result.write(intToByteArray(0));
        result.write(LIST_LEVEL_FOLOW);
        result.write(intToByteArray(levelFollowValue));
        result.write(LIST_LEVEL_START_AT);
        result.write(intToByteArray(this.listStartAt));
        if(this.isTentative) {
            result.write(LIST_LEVEL_TENTATIVE);
        }
        if(this.isLegal) {
            result.write(LIST_LEVEL_LEGAL);
        }
        result.write(LIST_LEVEL_SPACE);
        result.write(intToByteArray(0));
        result.write(LIST_LEVEL_INDENT);
        result.write(intToByteArray(0));
        if(levelPicture != -1) {
            result.write(LIST_LEVEL_PICTURE);
            result.write(intToByteArray(levelPicture));
        }
        
        result.write(OPEN_GROUP); 
        result.write(LIST_LEVEL_TEXT);
        result.write(LIST_LEVEL_TEMPLATE_ID);
        result.write(intToByteArray(this.templateID));
        
        
        if(this.listType != LIST_TYPE_BULLET) {
            result.write(LIST_LEVEL_STYLE_NUMBERED_BEGIN);
            if(this.levelTextNumber < 10) {
                result.write(intToByteArray(0));
            }
            result.write(intToByteArray(this.levelTextNumber));
            result.write(LIST_LEVEL_STYLE_NUMBERED_END);
        } else {
            result.write(LIST_LEVEL_STYLE_BULLETED_BEGIN);
            this.document.filterSpecialChar(result, this.bulletCharacter, false, false);
            result.write(LIST_LEVEL_STYLE_BULLETED_END);
        }
        result.write(CLOSE_GROUP);    
        
        result.write(OPEN_GROUP);  
        result.write(LIST_LEVEL_NUMBERS_BEGIN);
        if(this.listType != LIST_TYPE_BULLET) {
            result.write(LIST_LEVEL_NUMBERS_NUMBERED);
        }
        result.write(LIST_LEVEL_NUMBERS_END);
        result.write(CLOSE_GROUP);
        
        
        result.write(RtfFontList.FONT_NUMBER);
        if(this.listType != LIST_TYPE_BULLET) {
            result.write(intToByteArray(fontNumber.getFontNumber()));
        } else {
            result.write(intToByteArray(fontBullet.getFontNumber()));
        }
        result.write(DocWriter.getISOBytes("\\cf"));

        result.write(intToByteArray(document.getDocumentHeader().getColorNumber(new RtfColor(this.document,this.getFontNumber().getColor()))));
            
        writeIndentation(result);
        result.write(CLOSE_GROUP);
        this.document.outputDebugLinebreak(result);
        
    }
        
    public void writeContent(final OutputStream result) throws IOException
    {
    }     
    
    
    protected void writeListNumbers(final OutputStream result) throws IOException {

        if(listLevel > 0) {
            result.write(RtfList.LIST_LEVEL_NUMBER);
            result.write(intToByteArray(listLevel));
        }
    }
    
    
    
    public void writeIndentation(final OutputStream result) throws IOException {
        result.write(LIST_LEVEL_FIRST_INDENT);
        result.write(intToByteArray(firstIndent));
        result.write(RtfParagraphStyle.INDENT_LEFT);
        result.write(intToByteArray(leftIndent));
        result.write(RtfParagraphStyle.INDENT_RIGHT);
        result.write(intToByteArray(rightIndent));
        result.write(LIST_LEVEL_SYMBOL_INDENT);
        result.write(intToByteArray(this.leftIndent));

    }
    
    public void writeListBeginning(final OutputStream result) throws IOException {
        result.write(RtfParagraph.PARAGRAPH_DEFAULTS);
        if(this.inTable) {
            result.write(RtfParagraph.IN_TABLE);
        }
        switch (this.alignment) {
            case Element.ALIGN_LEFT:
                result.write(RtfParagraphStyle.ALIGN_LEFT);
                break;
            case Element.ALIGN_RIGHT:
                result.write(RtfParagraphStyle.ALIGN_RIGHT);
                break;
            case Element.ALIGN_CENTER:
                result.write(RtfParagraphStyle.ALIGN_CENTER);
                break;
            case Element.ALIGN_JUSTIFIED:
            case Element.ALIGN_JUSTIFIED_ALL:
                result.write(RtfParagraphStyle.ALIGN_JUSTIFY);
                break;
        }
        writeIndentation(result);
        result.write(RtfFont.FONT_SIZE);
        result.write(intToByteArray(fontNumber.getFontSize() * 2));
        if(this.symbolIndent > 0) {
            result.write(LIST_LEVEL_SYMBOL_INDENT);
            result.write(intToByteArray(this.leftIndent));
        }
    }
    
    protected void correctIndentation() {

        if(this.listLevelParent != null) {
            this.leftIndent = this.leftIndent + this.listLevelParent.getLeftIndent() + this.listLevelParent.getFirstIndent();
        }
    }
    
    public int getListLevel() {
        return listLevel;
    }
    
    
    
    public void setListLevel(int listLevel) {
        this.listLevel = listLevel;
    }
    
    
    public String getBulletCharacter() {
        return this.bulletCharacter;
    }
    
    public int getListStartAt() {
        return listStartAt;
    }
    
    public void setListStartAt(int listStartAt) {
        this.listStartAt = listStartAt;
    }

    
    public int getFirstIndent() {
        return firstIndent;
    }
    
    public void setFirstIndent(int firstIndent) {
        this.firstIndent = firstIndent;
    }
    
    public int getLeftIndent() {
        return leftIndent;
    }
    
    public void setLeftIndent(int leftIndent) {
        this.leftIndent = leftIndent;
    }
    
    public int getRightIndent() {
        return rightIndent;
    }
    
    public void setRightIndent(int rightIndent) {
        this.rightIndent = rightIndent;
    }
    
    public int getSymbolIndent() {
        return symbolIndent;
    }
    
    public void setSymbolIndent(int symbolIndent) {
        this.symbolIndent = symbolIndent;
    }
    
    public RtfList getParent() {
        return parent;
    }
    
    public void setParent(RtfList parent) {
        this.parent = parent;
    }
    
    public void setBulletCharacter(String bulletCharacter) {
        this.bulletCharacter = bulletCharacter;
    }
    
    public void setBulletChunk(Chunk bulletCharacter) {
        this.bulletChunk = bulletCharacter;
    }
    
    public int getListType() {
        return listType;
    }
    
    public void setListType(int listType) {
        this.listType = listType;
    }
    
    public void setBulletFont(Font f) {
        this.fontBullet = new RtfFont(document, f);
    }

    
    public RtfFont getFontNumber() {
        return fontNumber;
    }

    
    public void setFontNumber(RtfFont fontNumber) {
        this.fontNumber = fontNumber;
    }

    
    public RtfFont getFontBullet() {
        return fontBullet;
    }

    
    public void setFontBullet(RtfFont fontBullet) {
        this.fontBullet = fontBullet;
    }

    
    public boolean isTentative() {
        return isTentative;
    }

    
    public void setTentative(boolean isTentative) {
        this.isTentative = isTentative;
    }

    
    public boolean isLegal() {
        return isLegal;
    }

    
    public void setLegal(boolean isLegal) {
        this.isLegal = isLegal;
    }

    
    public int getLevelFollowValue() {
        return levelFollowValue;
    }

    
    public void setLevelFollowValue(int levelFollowValue) {
        this.levelFollowValue = levelFollowValue;
    }

    
    public int getLevelTextNumber() {
        return levelTextNumber;
    }

    
    public void setLevelTextNumber(int levelTextNumber) {
        this.levelTextNumber = levelTextNumber;
    }

    
    public RtfListLevel getListLevelParent() {
        return listLevelParent;
    }

    
    public void setListLevelParent(RtfListLevel listLevelParent) {
        this.listLevelParent = listLevelParent;
    }
}
