

package com.lowagie.text.rtf.list;

import java.awt.Color;
import java.io.IOException;
import java.io.OutputStream;
import java.security.InvalidParameterException;
import java.util.ArrayList;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocWriter;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.List;
import com.lowagie.text.ListItem;
import com.lowagie.text.Paragraph;
import com.lowagie.text.RomanList;
import com.lowagie.text.factories.RomanAlphabetFactory;
import com.lowagie.text.factories.RomanNumberFactory;
import com.lowagie.text.rtf.RtfBasicElement;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.RtfExtendedElement;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.style.RtfFont;
import com.lowagie.text.rtf.style.RtfFontList;
import com.lowagie.text.rtf.text.RtfParagraph;



public class RtfList extends RtfElement implements RtfExtendedElement {


    
    public static final byte[] LIST_NUMBER = DocWriter.getISOBytes("\\ls");

    
    private static final byte[] LIST = DocWriter.getISOBytes("\\list");
    
    public static final byte[] LIST_ID = DocWriter.getISOBytes("\\listid");
    
    private static final byte[] LIST_TEMPLATE_ID = DocWriter.getISOBytes("\\listtemplateid");
    
    private static final byte[] LIST_SIMPLE = DocWriter.getISOBytes("\\listsimple");
    
    private static final byte[] LIST_HYBRID = DocWriter.getISOBytes("\\listhybrid");
    
    private static final byte[] LIST_RESTARTHDN = DocWriter.getISOBytes("\\listrestarthdn");
    
    private static final byte[] LIST_NAME = DocWriter.getISOBytes("\\listname");
    
    private static final byte[] LIST_STYLEID = DocWriter.getISOBytes("\\liststyleid");
    
    private static final byte[] LIST_STYLENAME = DocWriter.getISOBytes("\\liststylename");

    
    
    public static final byte[] LIST_LEVEL_NUMBER = DocWriter.getISOBytes("\\ilvl");
    
    
    
    public static final byte[] LIST_TEXT = DocWriter.getISOBytes("\\listtext");
    
    public static final byte[] LIST_NUMBER_END = DocWriter.getISOBytes(".");
    
    

    
    public static final byte[] TAB = DocWriter.getISOBytes("\\tab");
    
    
    private ArrayList items;
    
    
    private RtfList parentList = null;

    
    private int listID = -1;
    
    
    public static final int LIST_TYPE_NORMAL = 0;                
    
    
    public static final int LIST_TYPE_SIMPLE = 1;                
    
    
    public static final int LIST_TYPE_HYBRID = 2;                
    
    
    private int listType = LIST_TYPE_HYBRID;
    
    
    private String name = null;
    
    
    private int listNumber = -1;

    
    private ArrayList listLevels = null;;

    
    
    public RtfList() {
        super(null);
        createDefaultLevels();
    }
    
    
    public void setDocument(RtfDocument doc) {
        this.document = doc;
        
        this.listNumber = document.getDocumentHeader().getListNumber(this); 

        
    }
    
    public RtfList(RtfDocument doc) {
        super(doc);
        createDefaultLevels();
        
        this.listNumber = document.getDocumentHeader().getListNumber(this); 

    }

    
    
    public RtfList(RtfDocument doc, List list) {
        
        
        
        
        
        
        super(doc);

        createDefaultLevels();
        
        this.items = new ArrayList();        
        RtfListLevel ll = (RtfListLevel)this.listLevels.get(0);
        
        
        this.listNumber = document.getDocumentHeader().getListNumber(this); 
        
        if(list.getSymbolIndent() > 0 && list.getIndentationLeft() > 0) {
            ll.setFirstIndent((int) (list.getSymbolIndent() * RtfElement.TWIPS_FACTOR * -1));
            ll.setLeftIndent((int) ((list.getIndentationLeft() + list.getSymbolIndent()) * RtfElement.TWIPS_FACTOR));
        } else if(list.getSymbolIndent() > 0) {
            ll.setFirstIndent((int) (list.getSymbolIndent() * RtfElement.TWIPS_FACTOR * -1));
            ll.setLeftIndent((int) (list.getSymbolIndent() * RtfElement.TWIPS_FACTOR));
        } else if(list.getIndentationLeft() > 0) {
            ll.setFirstIndent(0);
            ll.setLeftIndent((int) (list.getIndentationLeft() * RtfElement.TWIPS_FACTOR));
        } else {
            ll.setFirstIndent(0);
            ll.setLeftIndent(0);
        }
        ll.setRightIndent((int) (list.getIndentationRight() * RtfElement.TWIPS_FACTOR));
        ll.setSymbolIndent((int) ((list.getSymbolIndent() + list.getIndentationLeft()) * RtfElement.TWIPS_FACTOR));
        ll.correctIndentation();
        ll.setTentative(false);
        
        if (list instanceof RomanList) {
            if (list.isLowercase()) {
                ll.setListType(RtfListLevel.LIST_TYPE_LOWER_ROMAN);
            } else {
                ll.setListType(RtfListLevel.LIST_TYPE_UPPER_ROMAN);
            }
        } else if (list.isNumbered()) {
            ll.setListType(RtfListLevel.LIST_TYPE_NUMBERED);
        } else if (list.isLettered()) {
            if (list.isLowercase()) {
                ll.setListType(RtfListLevel.LIST_TYPE_LOWER_LETTERS);
            } else {
                ll.setListType(RtfListLevel.LIST_TYPE_UPPER_LETTERS);
            }
        } 
        else {





            ll.setBulletCharacter(list.getPreSymbol() + list.getSymbol().getContent() + list.getPostSymbol());
            ll.setListType(RtfListLevel.LIST_TYPE_BULLET);
        }
        
        
        for(int i = 0; i < list.getItems().size(); i++) {
            try {
                Element element = (Element) list.getItems().get(i);
                
                if(element.type() == Element.CHUNK) {
                    element = new ListItem((Chunk) element);
                }
                if(element instanceof ListItem) {
                    ll.setAlignment(((ListItem) element).getAlignment());
                }
                RtfBasicElement[] rtfElements = doc.getMapper().mapElement(element);
                for(int j = 0; j < rtfElements.length; j++) {
                    RtfBasicElement rtfElement = rtfElements[j];
                    if(rtfElement instanceof RtfList) {
                        ((RtfList) rtfElement).setParentList(this);
                    } else if(rtfElement instanceof RtfListItem) {
                        ((RtfListItem) rtfElement).setParent(ll);
                    }
                    ll.setFontNumber( new RtfFont(document, new Font(Font.TIMES_ROMAN, 10, Font.NORMAL, new Color(0, 0, 0))) );
                    if (list.getSymbol() != null && list.getSymbol().getFont() != null && !list.getSymbol().getContent().startsWith("-") && list.getSymbol().getContent().length() > 0) {
                        
                        ll.setBulletFont( list.getSymbol().getFont());
                        ll.setBulletCharacter(list.getSymbol().getContent().substring(0, 1));
                    } else
                     if (list.getSymbol() != null && list.getSymbol().getFont() != null) {
                         ll.setBulletFont(list.getSymbol().getFont());
                     
                     } else {
                        ll.setBulletFont(new Font(Font.SYMBOL, 10, Font.NORMAL, new Color(0, 0, 0)));
                    } 
                    items.add(rtfElement);
                }

            } catch(DocumentException de) {
                de.printStackTrace();
            }
        }
    }
    
    
    public void writeDefinition(final OutputStream result) throws IOException
    {
        result.write(OPEN_GROUP);
        result.write(LIST);
        result.write(LIST_TEMPLATE_ID);
        result.write(intToByteArray(document.getRandomInt()));

        int levelsToWrite = -1;
        
        switch(this.listType) {
        case LIST_TYPE_NORMAL:
            levelsToWrite = listLevels.size();
            break;
        case LIST_TYPE_SIMPLE:
            result.write(LIST_SIMPLE);
            result.write(intToByteArray(1)); 
            levelsToWrite = 1;
            break;
        case LIST_TYPE_HYBRID:
            result.write(LIST_HYBRID);
            levelsToWrite = listLevels.size();
            break;
        default:
            break;
        }
        this.document.outputDebugLinebreak(result);

        
        
        
        
        
        
        
        
        
        
         
        
        for(int i = 0; i<levelsToWrite; i++) {
            ((RtfListLevel)listLevels.get(i)).writeDefinition(result);
            this.document.outputDebugLinebreak(result);
        }
        
        result.write(LIST_ID);
        result.write(intToByteArray(this.listID));
        result.write(CLOSE_GROUP);
        this.document.outputDebugLinebreak(result);
        if(items != null) {
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
    }
    
        
    public void writeContent(final OutputStream result) throws IOException
    {
        if(!this.inTable) {
            result.write(OPEN_GROUP);
        }
        
        int itemNr = 0;
        if(items != null) {
        for(int i = 0; i < items.size(); i++) {
            
            RtfElement thisRtfElement = (RtfElement) items.get(i);
           
            if(thisRtfElement instanceof RtfListItem) {
                itemNr++;
                RtfListItem rtfElement = (RtfListItem)thisRtfElement;
                RtfListLevel listLevel =  rtfElement.getParent();
                if(listLevel.getListLevel() == 0) {
                    correctIndentation();
                }
                
                if(i == 0) {
                    listLevel.writeListBeginning(result);
                    writeListNumbers(result);
                }

                writeListTextBlock(result, itemNr, listLevel);
                
                rtfElement.writeContent(result);
                
                if(i < (items.size() - 1) || !this.inTable || listLevel.getListType() > 0) { 
                    result.write(RtfParagraph.PARAGRAPH);
                }
                this.document.outputDebugLinebreak(result);
            } else if(thisRtfElement instanceof RtfList) {
                ((RtfList)thisRtfElement).writeContent(result);

                writeListNumbers(result);
                this.document.outputDebugLinebreak(result);
            }
        }
        }
        if(!this.inTable) {
            result.write(CLOSE_GROUP);
            result.write(RtfParagraph.PARAGRAPH_DEFAULTS);
        }
    }        
    
    protected void writeListTextBlock(final OutputStream result, int itemNr, RtfListLevel listLevel) 
    throws IOException {
        result.write(OPEN_GROUP);
        result.write(RtfList.LIST_TEXT);
        result.write(RtfParagraph.PARAGRAPH_DEFAULTS);
        if(this.inTable) {
            result.write(RtfParagraph.IN_TABLE);
        }
        result.write(RtfFontList.FONT_NUMBER);
        if(listLevel.getListType() != RtfListLevel.LIST_TYPE_BULLET) {
            result.write(intToByteArray(listLevel.getFontNumber().getFontNumber()));
        } else {
            result.write(intToByteArray(listLevel.getFontBullet().getFontNumber()));
        }
        listLevel.writeIndentation(result);
        result.write(DELIMITER);
        if(listLevel.getListType() != RtfListLevel.LIST_TYPE_BULLET) {
            switch(listLevel.getListType()) {
                case RtfListLevel.LIST_TYPE_NUMBERED      : result.write(intToByteArray(itemNr)); break;
                case RtfListLevel.LIST_TYPE_UPPER_LETTERS : result.write(DocWriter.getISOBytes(RomanAlphabetFactory.getUpperCaseString(itemNr))); break;
                case RtfListLevel.LIST_TYPE_LOWER_LETTERS : result.write(DocWriter.getISOBytes(RomanAlphabetFactory.getLowerCaseString(itemNr))); break;
                case RtfListLevel.LIST_TYPE_UPPER_ROMAN   : result.write(DocWriter.getISOBytes(RomanNumberFactory.getUpperCaseString(itemNr))); break;
                case RtfListLevel.LIST_TYPE_LOWER_ROMAN   : result.write(DocWriter.getISOBytes(RomanNumberFactory.getLowerCaseString(itemNr))); break;
            }
            result.write(LIST_NUMBER_END);
        } else {
            this.document.filterSpecialChar(result, listLevel.getBulletCharacter(), true, false);
        }
        result.write(TAB);
        result.write(CLOSE_GROUP);
    }

    
    protected void writeListNumbers(final OutputStream result) throws IOException {
        result.write(RtfList.LIST_NUMBER);
        result.write(intToByteArray(listNumber));
    }
    
    protected void createDefaultLevels() {
        this.listLevels = new ArrayList();    
        for(int i=0; i<=8; i++) {
            
            RtfListLevel ll = new RtfListLevel(this.document);
            ll.setListType(RtfListLevel.LIST_TYPE_NUMBERED);
            ll.setFirstIndent(0);
            ll.setLeftIndent(0);
            ll.setLevelTextNumber(i);
            ll.setTentative(true);
            ll.correctIndentation();
            this.listLevels.add(ll);
        }

    }
    
    public int getListNumber() {
        return listNumber;
    }
    
    
    public void setListNumber(int listNumber) {
        this.listNumber = listNumber;
    }
    
    
    public void setInTable(boolean inTable) {
        super.setInTable(inTable);
        for(int i = 0; i < this.items.size(); i++) {
            ((RtfBasicElement) this.items.get(i)).setInTable(inTable);
        }
    }
    
    
    public void setInHeader(boolean inHeader) {
        super.setInHeader(inHeader);
        for(int i = 0; i < this.items.size(); i++) {
            ((RtfBasicElement) this.items.get(i)).setInHeader(inHeader);
        }
    }

    
    protected void correctIndentation() {
        



        for(int i = 0; i < this.items.size(); i++) {
            if(this.items.get(i) instanceof RtfList) {
                ((RtfList) this.items.get(i)).correctIndentation();
            } else if(this.items.get(i) instanceof RtfListItem) {
                ((RtfListItem) this.items.get(i)).correctIndentation();
            }
        }
    }


    
    public void setID(int id) {
        this.listID = id;
    }
    
    public int getID() {
        return this.listID;
    }

    
    public int getListType() {
        return listType;
    }

    
    public void setListType(int listType) throws InvalidParameterException {
        if(listType == LIST_TYPE_NORMAL || 
                listType == LIST_TYPE_SIMPLE || 
                listType == LIST_TYPE_HYBRID ) {
            this.listType = listType;
        }
        else {
            throw new InvalidParameterException("Invalid listType value.");
        }
    }

    
    public RtfList getParentList() {
        return parentList;
    }

    
    public void setParentList(RtfList parentList) {
        this.parentList = parentList;
    }

    
    public String getName() {
        return name;
    }

    
    public void setName(String name) {
        this.name = name;
    }
    
    public RtfListLevel getListLevel(int index) {
        if(listLevels != null) {
        return (RtfListLevel)this.listLevels.get(index);
        }
        else
            return null;
    }

}
