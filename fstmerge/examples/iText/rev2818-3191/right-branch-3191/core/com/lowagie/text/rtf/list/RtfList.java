

package com.lowagie.text.rtf.list;

import java.awt.Color;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.List;
import com.lowagie.text.ListItem;
import com.lowagie.text.RomanList;
import com.lowagie.text.factories.RomanAlphabetFactory;
import com.lowagie.text.factories.RomanNumberFactory;
import com.lowagie.text.rtf.RtfBasicElement;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.RtfExtendedElement;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.style.RtfFont;
import com.lowagie.text.rtf.style.RtfFontList;
import com.lowagie.text.rtf.style.RtfParagraphStyle;
import com.lowagie.text.rtf.text.RtfParagraph;



public class RtfList extends RtfElement implements RtfExtendedElement {

    
    private static final byte[] LIST_LEVEL = "\\listlevel".getBytes();
    
    private static final byte[] LIST_LEVEL_TYPE = "\\levelnfc".getBytes();
    
    private static final byte[] LIST_LEVEL_TYPE_NEW = "\\levelnfcn".getBytes();
    
    private static final byte[] LIST_LEVEL_ALIGNMENT = "\\leveljc".getBytes();
    
    private static final byte[] LIST_LEVEL_ALIGNMENT_NEW = "\\leveljcn".getBytes();
    
    private static final byte[] LIST_LEVEL_START_AT = "\\levelstartat".getBytes();
    
    private static final byte[] LIST_LEVEL_TEXT = "\\leveltext".getBytes();
    
    private static final byte[] LIST_LEVEL_STYLE_NUMBERED_BEGIN = "\\\'02\\\'".getBytes();
    
    private static final byte[] LIST_LEVEL_STYLE_NUMBERED_END = ".;".getBytes();
    
    private static final byte[] LIST_LEVEL_STYLE_BULLETED_BEGIN = "\\\'01".getBytes();
    
    private static final byte[] LIST_LEVEL_STYLE_BULLETED_END = ";".getBytes();
    
    private static final byte[] LIST_LEVEL_NUMBERS_BEGIN = "\\levelnumbers".getBytes();
    
    private static final byte[] LIST_LEVEL_NUMBERS_NUMBERED = "\\\'01".getBytes();
    
    private static final byte[] LIST_LEVEL_NUMBERS_END = ";".getBytes();
    
    private static final byte[] LIST_LEVEL_FIRST_INDENT = "\\fi".getBytes();
    
    private static final byte[] LIST_LEVEL_SYMBOL_INDENT = "\\tx".getBytes();
    
    private static final byte[] LIST_LEVEL_NUMBER = "\\ilvl".getBytes();
    
    private static final byte[] TAB = "\\tab".getBytes();
    
    private static final byte[] LIST_TEXT = "\\listtext".getBytes();
    
    private static final byte[] LIST_NUMBER_END = ".".getBytes();
    
    private static final byte[] LIST_BULLET = "\\\'b7".getBytes();
    
    private static final int LIST_TYPE_BULLET = 0;
    private static final int LIST_TYPE_NUMBERED = 1;
    private static final int LIST_TYPE_UPPER_LETTERS = 2;
    private static final int LIST_TYPE_LOWER_LETTERS = 3;
    private static final int LIST_TYPE_UPPER_ROMAN = 4;
    private static final int LIST_TYPE_LOWER_ROMAN = 5;
    
    
    private ArrayList<RtfBasicElement> items;
    
    private int listLevel = 0;
    
    private int firstIndent = 0;
    
    private int leftIndent = 0;
    
    private int rightIndent = 0;
    
    private int symbolIndent = 0;
    
    private int listNumber = 1;
    
    private int listType = LIST_TYPE_BULLET;
    
    private RtfFont fontNumber;
    
    private RtfFont fontBullet;
    
    private int alignment = Element.ALIGN_LEFT;
    
    private RtfList parentList = null;
    
    private String bulletCharacter = "\u"; 
    
    
    public RtfList(RtfDocument doc, List list) {
        super(doc);
        
        this.listNumber = document.getDocumentHeader().getListNumber(this);
        
        this.items = new ArrayList<RtfBasicElement>();
        if(list.getSymbolIndent() > 0 && list.getIndentationLeft() > 0) {
            this.firstIndent = (int) (list.getSymbolIndent() * RtfElement.TWIPS_FACTOR * -1);
            this.leftIndent = (int) ((list.getIndentationLeft() + list.getSymbolIndent()) * RtfElement.TWIPS_FACTOR);
        } else if(list.getSymbolIndent() > 0) {
            this.firstIndent = (int) (list.getSymbolIndent() * RtfElement.TWIPS_FACTOR * -1);
            this.leftIndent = (int) (list.getSymbolIndent() * RtfElement.TWIPS_FACTOR);
        } else if(list.getIndentationLeft() > 0) {
            this.firstIndent = 0;
            this.leftIndent = (int) (list.getIndentationLeft() * RtfElement.TWIPS_FACTOR);
        } else {
            this.firstIndent = 0;
            this.leftIndent = 0;
        }
        this.rightIndent = (int) (list.getIndentationRight() * RtfElement.TWIPS_FACTOR);
        this.symbolIndent = (int) ((list.getSymbolIndent() + list.getIndentationLeft()) * RtfElement.TWIPS_FACTOR);
        
        if(list instanceof RomanList) {
            if(list.isLowercase()) {
                this.listType = LIST_TYPE_LOWER_ROMAN;
            } else {
                this.listType = LIST_TYPE_UPPER_ROMAN;
            }
        } else if(list.isNumbered()) {
            this.listType = LIST_TYPE_NUMBERED;
        } else if(list.isLettered()) {
            if(list.isLowercase()) {
                this.listType = LIST_TYPE_LOWER_LETTERS;
            } else {
                this.listType = LIST_TYPE_UPPER_LETTERS;
            }
        }
        
        for(int i = 0; i < list.getItems().size(); i++) {
            try {
                Element element = list.getItems().get(i);
                if(element.type() == Element.CHUNK) {
                    element = new ListItem((Chunk) element);
                }
                if(element instanceof ListItem) {
                    this.alignment = ((ListItem) element).getAlignment();
                }
                RtfBasicElement rtfElement = doc.getMapper().mapElement(element);
                if(rtfElement instanceof RtfList) {
                    ((RtfList) rtfElement).setListNumber(listNumber);
                    ((RtfList) rtfElement).setListLevel(listLevel + 1);
                    ((RtfList) rtfElement).setParent(this);
                } else if(rtfElement instanceof RtfListItem) {
                    ((RtfListItem) rtfElement).setParent(this);
                    ((RtfListItem) rtfElement).inheritListSettings(listNumber, listLevel + 1);
                }
                items.add(rtfElement);
            } catch(DocumentException de) {
                de.printStackTrace();
            }
        }
        
        if(this.listLevel == 0) {
            correctIndentation();
        }
        
        fontNumber = new RtfFont(document, new Font(Font.TIMES_ROMAN, 10, Font.NORMAL, new Color(0, 0, 0)));
        if (list.getSymbol() != null && list.getSymbol().getFont() != null && !list.getSymbol().getContent().startsWith("-") && list.getSymbol().getContent().length() > 0) {
            
            this.fontBullet = new RtfFont(document, list.getSymbol().getFont());
            this.bulletCharacter = list.getSymbol().getContent().substring(0, 1);
        } else {
            this.fontBullet = new RtfFont(document, new Font(Font.SYMBOL, 10, Font.NORMAL, new Color(0, 0, 0)));
        }        
    }
    
    
    private byte[] writeIndentations() {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            writeIndentation(result);
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }
    
    
    private void writeIndentations(final OutputStream result) throws IOException {
        writeIndentation(result);
    }
    
    
    private void writeIndentation(final OutputStream result) throws IOException {
        result.write(LIST_LEVEL_FIRST_INDENT);
        result.write(intToByteArray(firstIndent));
        result.write(RtfParagraphStyle.INDENT_LEFT);
        result.write(intToByteArray(leftIndent));
        result.write(RtfParagraphStyle.INDENT_RIGHT);
        result.write(intToByteArray(rightIndent));        
    }
    
    
    public byte[] writeDefinition()
    {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            writeDefinition(result);
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }
    
    public void writeDefinition(final OutputStream result) throws IOException
    {
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
        }
        result.write(LIST_LEVEL_TYPE_NEW);
        switch(this.listType) {
            case LIST_TYPE_BULLET        : result.write(intToByteArray(23)); break;
            case LIST_TYPE_NUMBERED      : result.write(intToByteArray(0)); break;
            case LIST_TYPE_UPPER_LETTERS : result.write(intToByteArray(3)); break;
            case LIST_TYPE_LOWER_LETTERS : result.write(intToByteArray(4)); break;
            case LIST_TYPE_UPPER_ROMAN   : result.write(intToByteArray(1)); break;
            case LIST_TYPE_LOWER_ROMAN   : result.write(intToByteArray(2)); break;
        }
        result.write(LIST_LEVEL_ALIGNMENT);
        result.write(intToByteArray(0));
        result.write(LIST_LEVEL_ALIGNMENT_NEW);
        result.write(intToByteArray(0));
        result.write(LIST_LEVEL_START_AT);
        result.write(intToByteArray(1));
        result.write(OPEN_GROUP);
        result.write(LIST_LEVEL_TEXT);
        if(this.listType != LIST_TYPE_BULLET) {
            result.write(LIST_LEVEL_STYLE_NUMBERED_BEGIN);
            if(listLevel < 10) {
                result.write(intToByteArray(0));
            }
            result.write(intToByteArray(listLevel));
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
        
        writeIndentation(result);
        result.write(LIST_LEVEL_SYMBOL_INDENT);
        result.write(intToByteArray(this.leftIndent));
        result.write(CLOSE_GROUP);
        result.write("\n".getBytes());
        for(int i = 0; i < items.size(); i++) {
            RtfElement rtfElement = (RtfElement) items.get(i);
            if(rtfElement instanceof RtfList) {
                RtfList rl = (RtfList)rtfElement;
                
                rl.writeDefinition(result);
                break;
            } else if(rtfElement instanceof RtfListItem) {
                RtfListItem rli = (RtfListItem) rtfElement;
                
                
                
                
                
                if(rli.writeDefinition(result)) break;
            }
        }        
    }

    
    
    protected byte[] writeListBeginning() {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
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
                result.write("\\tx".getBytes());
                result.write(intToByteArray(this.leftIndent));
            }
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }

    
    protected byte[] writeListNumbers() {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            result.write(RtfListTable.LIST_NUMBER);
            result.write(intToByteArray(listNumber));
            if(listLevel > 0) {
                result.write(LIST_LEVEL_NUMBER);
                result.write(intToByteArray(listLevel));
            }
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }
    
    
    public byte[] write()  
    {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            writeContent(result);
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }
    
    
        
    public void writeContent(final OutputStream result) throws IOException
    {
        result.write(OPEN_GROUP);
        int itemNr = 0;
        for(int i = 0; i < items.size(); i++) {
            RtfElement rtfElement = (RtfElement) items.get(i);
            if(rtfElement instanceof RtfListItem) {
                itemNr++;
                result.write(writeListBeginning());
                result.write(writeListNumbers());
                result.write(OPEN_GROUP);
                result.write(LIST_TEXT);
                result.write(RtfParagraph.PARAGRAPH_DEFAULTS);
                if(this.inTable) {
                    result.write(RtfParagraph.IN_TABLE);
                }
                result.write(RtfFontList.FONT_NUMBER);
                if(this.listType != LIST_TYPE_BULLET) {
                    result.write(intToByteArray(fontNumber.getFontNumber()));
                } else {
                    result.write(intToByteArray(fontBullet.getFontNumber()));
                }
                
                writeIndentation(result);
                result.write(DELIMITER);
                if(this.listType != LIST_TYPE_BULLET) {
                    switch(this.listType) {
                        case LIST_TYPE_NUMBERED      : result.write(intToByteArray(itemNr)); break;
                        case LIST_TYPE_UPPER_LETTERS : result.write(RomanAlphabetFactory.getUpperCaseString(itemNr).getBytes()); break;
                        case LIST_TYPE_LOWER_LETTERS : result.write(RomanAlphabetFactory.getLowerCaseString(itemNr).getBytes()); break;
                        case LIST_TYPE_UPPER_ROMAN   : result.write(RomanNumberFactory.getUpperCaseString(itemNr).getBytes()); break;
                        case LIST_TYPE_LOWER_ROMAN   : result.write(RomanNumberFactory.getLowerCaseString(itemNr).getBytes()); break;
                    }
                    result.write(LIST_NUMBER_END);
                } else {
                    this.document.filterSpecialChar(result, this.bulletCharacter, true, false);
                }
                result.write(TAB);
                result.write(CLOSE_GROUP);                
                
                rtfElement.writeContent(result);
                if(i < (items.size() - 1) || !this.inTable || this.listLevel > 0) { 
                    result.write(RtfParagraph.PARAGRAPH);
                }
                result.write("\n".getBytes());
            } else if(rtfElement instanceof RtfList) {
                
                rtfElement.writeContent(result);
                result.write("\n".getBytes());
            }
        }
        result.write(CLOSE_GROUP);
        
        if(!this.inTable) {
            result.write(RtfParagraph.PARAGRAPH_DEFAULTS);
        }
    }        
    
    
    public int getListLevel() {
        return listLevel;
    }
    
    
    public void setListLevel(int listLevel) {
        this.listLevel = listLevel;
        if(this.listLevel != 0) {
            document.getDocumentHeader().freeListNumber(this);
            for(RtfBasicElement e: this.items) {
                if(e instanceof RtfList) {
                    RtfList l = (RtfList) e;
                    l.setListNumber(this.listNumber);
                    l.setListLevel(this.listLevel + 1);
                }
            }
        } else {
            this.listNumber = document.getDocumentHeader().getListNumber(this);
        }
    }
    
    
    protected void setParent(RtfList parent) {
        this.parentList = parent;
    }
    
    
    public int getListNumber() {
        return listNumber;
    }
    
    
    public void setListNumber(int listNumber) {
        this.listNumber = listNumber;
    }
    
    
    public void setInTable(boolean inTable) {
        super.setInTable(inTable);
        for(RtfBasicElement e: this.items) {
            e.setInTable(inTable);
        }
    }
    
    
    public void setInHeader(boolean inHeader) {
        super.setInHeader(inHeader);
        for(RtfBasicElement e: this.items) {
            e.setInHeader(inHeader);
        }
    }

    
    protected void correctIndentation() {
        if(this.parentList != null) {
            this.leftIndent = this.leftIndent + this.parentList.getLeftIndent() + this.parentList.getFirstIndent();
        }
        for(int i = 0; i < this.items.size(); i++) {
            if(this.items.get(i) instanceof RtfList) {
                ((RtfList) this.items.get(i)).correctIndentation();
            } else if(this.items.get(i) instanceof RtfListItem) {
                ((RtfListItem) this.items.get(i)).correctIndentation();
            }
        }
    }

    
    private int getLeftIndent() {
        return this.leftIndent;
    }
    
    
    private int getFirstIndent() {
        return this.firstIndent;
    }
}
