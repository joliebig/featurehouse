
package com.lowagie.text.rtf.parser.destinations;

import java.awt.Color;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Font;
import com.lowagie.text.FontFactory;
import com.lowagie.text.Paragraph;
import com.lowagie.text.rtf.direct.RtfDirectContent;
import com.lowagie.text.rtf.document.*;
import com.lowagie.text.rtf.parser.RtfParser;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordData;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordType;
import com.lowagie.text.rtf.parser.properties.RtfProperty;
import com.lowagie.text.rtf.parser.properties.RtfPropertyListener;

public final class RtfDestinationDocument extends RtfDestination implements RtfPropertyListener {


    
    private RtfDocument rtfDoc = null;
    
    
    private Document doc = null;
    
    private StringBuffer buffer = null;
    
    private int conversionType = 0;
    
    
    
    private int tableLevel = 0;
    
    private static final List IMPORT_IGNORED_CTRLWORDS = Arrays.asList(new String[]{
        "rtf",
        "ansicpg",
        "deff",
        "ansi",
        "mac",
        "pca",
        "pc",
        "stshfdbch",
        "stshfloch",
        "stshfhich",
        "stshfbi",
        "deflang",
        "deflangfe",
        "adeflang",
        "adeflangfe"});

    private static final List CONVERT_IGNORED_CTRLWORDS = Arrays.asList(new String[]{"rtf"});

    private Paragraph iTextParagraph = null;
    
    public RtfDestinationDocument() {
        super(null);
    }
    
    public RtfDestinationDocument(RtfParser parser) {
        super(parser);
        this.rtfDoc = parser.getRtfDocument();
        this.doc = parser.getDocument();
        this.conversionType = parser.getConversionType();
        setToDefaults();
        if(this.rtfParser.isConvert()) {
            this.rtfParser.getState().properties.addRtfPropertyListener(this);
        }
    }
    
    
    
    protected void finalize() throws Throwable {
        
        if(this.rtfParser.isConvert()) {
            this.rtfParser.getState().properties.removeRtfPropertyListener(this);
        }
        super.finalize();
    }

    public void setParser(RtfParser parser) {
        this.rtfParser = parser;
        this.rtfDoc = parser.getRtfDocument();
        this.doc = parser.getDocument();
        this.conversionType = parser.getConversionType();
        setToDefaults();
        if(this.rtfParser.isConvert()) {
            this.rtfParser.getState().properties.addRtfPropertyListener(this);
        }
    }
    

    
    public boolean closeDestination() {
        if(this.rtfParser.isImport()) {
            if(this.buffer.length()>0) {
                writeBuffer();
            }
        }
        
        this.rtfParser.getState().properties.removeRtfPropertyListener(this);

        return true;
    }

    
    public boolean handleOpenGroup() {
        this.onOpenGroup();    
        
        if(this.rtfParser.isImport()) {
        }
        if(this.rtfParser.isConvert()) {
            if(this.iTextParagraph == null) this.iTextParagraph = new Paragraph();
        }
        return true;
    }
    
    public boolean handleOpeningSubGroup() {
        if(this.rtfParser.isImport()) {
            if(this.buffer.length()>0) {
                writeBuffer();
            }
        }
        return true;
    }
    
    public boolean handleCloseGroup() {
        this.onCloseGroup();    
        
        if(this.rtfParser.isImport()) {
            if(this.buffer.length()>0) {
                writeBuffer();
            }
            writeText("}");
        }
        if(this.rtfParser.isConvert()) {
            if(this.buffer.length() > 0 && this.iTextParagraph == null) {
                this.iTextParagraph = new Paragraph();
            }
            if(this.buffer.length() > 0 ) {
                Chunk chunk = new Chunk();
                chunk.append(this.buffer.toString());
                this.iTextParagraph.add(chunk);
            }
            if(this.iTextParagraph != null) {
                addParagraphToDocument();
            }
        }
        return true;
    }
    
    public boolean handleCharacter(int ch) {
        boolean result = true;
        this.onCharacter(ch);    
        
        if(this.rtfParser.isImport()) {
            if(buffer.length() > 254) {
                this.writeBuffer();
            }
            buffer.append((char)ch);
        }
        if(this.rtfParser.isConvert()) {
            buffer.append((char)ch);
        }
        return result;
    }

    
    public boolean handleControlWord(RtfCtrlWordData ctrlWordData) {
        boolean result = false;
        this.onCtrlWord(ctrlWordData);    
        
        if(this.rtfParser.isImport()) {
            
            if(ctrlWordData.ctrlWord.equals("f")) { ctrlWordData.param =  this.rtfParser.getImportManager().mapFontNr(ctrlWordData.param);}
            
            
            
            if(ctrlWordData.ctrlWord.equals("cb")) { ctrlWordData.param = this.rtfParser.getImportManager().mapColorNr(ctrlWordData.param);}
            if(ctrlWordData.ctrlWord.equals("cf")) { ctrlWordData.param = this.rtfParser.getImportManager().mapColorNr(ctrlWordData.param);}
            
            if(ctrlWordData.ctrlWord.equals("clcbpat")) { ctrlWordData.param = this.rtfParser.getImportManager().mapColorNr(ctrlWordData.param);}
            if(ctrlWordData.ctrlWord.equals("clcbpatraw")) { ctrlWordData.param = this.rtfParser.getImportManager().mapColorNr(ctrlWordData.param);}
            if(ctrlWordData.ctrlWord.equals("clcfpat")) { ctrlWordData.param = this.rtfParser.getImportManager().mapColorNr(ctrlWordData.param);}
            if(ctrlWordData.ctrlWord.equals("clcfpatraw")) { ctrlWordData.param = this.rtfParser.getImportManager().mapColorNr(ctrlWordData.param);}
            
            if(ctrlWordData.ctrlWord.equals("trcfpat")) { ctrlWordData.param = this.rtfParser.getImportManager().mapColorNr(ctrlWordData.param);}
            if(ctrlWordData.ctrlWord.equals("trcbpat")) { ctrlWordData.param = this.rtfParser.getImportManager().mapColorNr(ctrlWordData.param);}
            
            if(ctrlWordData.ctrlWord.equals("brdrcf")) { ctrlWordData.param = this.rtfParser.getImportManager().mapColorNr(ctrlWordData.param);}
            
            if(ctrlWordData.ctrlWord.equals("ls")) { ctrlWordData.param = this.rtfParser.getImportManager().mapListNr(ctrlWordData.param);}
        }
        

        
        if(this.rtfParser.isConvert()) {
            if(ctrlWordData.ctrlWord.equals("par")) { addParagraphToDocument(); }
            
            if(ctrlWordData.ctrlWord.equals("f")) {}
            
            
            
            if(ctrlWordData.ctrlWord.equals("cb")) {}
            if(ctrlWordData.ctrlWord.equals("cf")) {}
            
            if(ctrlWordData.ctrlWord.equals("clcbpat")) {}
            if(ctrlWordData.ctrlWord.equals("clcbpatraw")) {}
            if(ctrlWordData.ctrlWord.equals("clcfpat")) {}
            if(ctrlWordData.ctrlWord.equals("clcfpatraw")) {}
            
            if(ctrlWordData.ctrlWord.equals("trcfpat")) {}
            if(ctrlWordData.ctrlWord.equals("trcbpat")) {}
            
            if(ctrlWordData.ctrlWord.equals("brdrcf")) {}
            
            
            if(ctrlWordData.ctrlWord.equals("trowd"))  { tableLevel++;}
            if(ctrlWordData.ctrlWord.equals("cell"))  {


            }
            if(ctrlWordData.ctrlWord.equals("row"))  { tableLevel++;}
            if(ctrlWordData.ctrlWord.equals("lastrow"))  {}
            if(ctrlWordData.ctrlWord.equals("row"))  { tableLevel++;}
            if(ctrlWordData.ctrlWord.equals("irow"))  {}
            if(ctrlWordData.ctrlWord.equals("irowband"))  {}
            if(ctrlWordData.ctrlWord.equals("tcelld"))  {}
            if(ctrlWordData.ctrlWord.equals("nestcell"))  {}
            if(ctrlWordData.ctrlWord.equals("nestrow"))  {}
            if(ctrlWordData.ctrlWord.equals("nesttableprops"))  {}
            if(ctrlWordData.ctrlWord.equals("nonesttables"))  {}
            if(ctrlWordData.ctrlWord.equals("trgaph"))  {}
            if(ctrlWordData.ctrlWord.equals("cellx"))  {}
            if(ctrlWordData.ctrlWord.equals("clmgf"))  {}
            if(ctrlWordData.ctrlWord.equals("clmrg"))  {}
            if(ctrlWordData.ctrlWord.equals("clvmgf"))  {}
            if(ctrlWordData.ctrlWord.equals("clvmrg"))  {}
            
            if(ctrlWordData.ctrlWord.equals("trauth"))  {}
            if(ctrlWordData.ctrlWord.equals("trdate"))  {}
            
            if(ctrlWordData.ctrlWord.equals("tbllkborder"))  {}
            if(ctrlWordData.ctrlWord.equals("tbllkshading"))  {}
            if(ctrlWordData.ctrlWord.equals("tbllkfont"))  {}
            if(ctrlWordData.ctrlWord.equals("tbllkcolor"))  {}
            if(ctrlWordData.ctrlWord.equals("tbllkbestfit"))  {}
            if(ctrlWordData.ctrlWord.equals("tbllkhdrrows"))  {}
            if(ctrlWordData.ctrlWord.equals("tbllklastrow"))  {}
            if(ctrlWordData.ctrlWord.equals("tbllkhdrcols"))  {}
            if(ctrlWordData.ctrlWord.equals("tbllklastcol"))  {}
            if(ctrlWordData.ctrlWord.equals("tbllknorowband"))  {}
            if(ctrlWordData.ctrlWord.equals("tbllknocolband"))  {}
            
            if(ctrlWordData.ctrlWord.equals("taprtl"))  {}
            if(ctrlWordData.ctrlWord.equals("trautofit"))  {}
            if(ctrlWordData.ctrlWord.equals("trhdr"))  {}
            if(ctrlWordData.ctrlWord.equals("trkeep"))  {}
            if(ctrlWordData.ctrlWord.equals("trkeepfollow"))  {}
            if(ctrlWordData.ctrlWord.equals("trleft"))  {}
            if(ctrlWordData.ctrlWord.equals("trqc"))  {}
            if(ctrlWordData.ctrlWord.equals("trql"))  {}
            if(ctrlWordData.ctrlWord.equals("trqr"))  {}
            if(ctrlWordData.ctrlWord.equals("trrh"))  {}
            if(ctrlWordData.ctrlWord.equals("trpaddb"))  {}
            if(ctrlWordData.ctrlWord.equals("trpaddl"))  {}
            if(ctrlWordData.ctrlWord.equals("trpaddr"))  {}
            if(ctrlWordData.ctrlWord.equals("trpaddt"))  {}
            if(ctrlWordData.ctrlWord.equals("trpaddfb"))  {}
            if(ctrlWordData.ctrlWord.equals("trpaddfl"))  {}
            if(ctrlWordData.ctrlWord.equals("trpaddfr"))  {}
            if(ctrlWordData.ctrlWord.equals("trpaddft"))  {}
            if(ctrlWordData.ctrlWord.equals("trspdl"))  {}
            if(ctrlWordData.ctrlWord.equals("trspdt"))  {}
            if(ctrlWordData.ctrlWord.equals("trspdb"))  {}
            if(ctrlWordData.ctrlWord.equals("trspdr"))  {}
            if(ctrlWordData.ctrlWord.equals("trspdfl"))  {}
            if(ctrlWordData.ctrlWord.equals("trspdft"))  {}
            if(ctrlWordData.ctrlWord.equals("trspdfb"))  {}
            if(ctrlWordData.ctrlWord.equals("trspdfr"))  {}
            if(ctrlWordData.ctrlWord.equals("trwWidth"))  {}
            if(ctrlWordData.ctrlWord.equals("trftsWidth"))  {}
            if(ctrlWordData.ctrlWord.equals("trwWidthB"))  {}
            if(ctrlWordData.ctrlWord.equals("trftsWidthB"))  {}
            if(ctrlWordData.ctrlWord.equals("trftsWidthB"))  {}
            if(ctrlWordData.ctrlWord.equals("trwWidthA"))  {}
            if(ctrlWordData.ctrlWord.equals("trftsWidthA"))  {}
            if(ctrlWordData.ctrlWord.equals("tblind"))  {}
            if(ctrlWordData.ctrlWord.equals("tblindtype"))  {}
            
            if(ctrlWordData.ctrlWord.equals("trcbpat"))  {}
            if(ctrlWordData.ctrlWord.equals("trcfpat"))  {}
            if(ctrlWordData.ctrlWord.equals("trpat"))  {}
            if(ctrlWordData.ctrlWord.equals("trshdng"))  {}
            if(ctrlWordData.ctrlWord.equals("trbgbdiag"))  {}
            if(ctrlWordData.ctrlWord.equals("trbgcross"))  {}
            if(ctrlWordData.ctrlWord.equals("trbgdcross"))  {}
            if(ctrlWordData.ctrlWord.equals("trbgdkbdiag"))  {}
            if(ctrlWordData.ctrlWord.equals("trbgdkcross"))  {}
            if(ctrlWordData.ctrlWord.equals("trbgdkdcross"))  {}
            if(ctrlWordData.ctrlWord.equals("trbgdkfdiag"))  {}
            if(ctrlWordData.ctrlWord.equals("trbgdkhor"))  {}
            if(ctrlWordData.ctrlWord.equals("trbgdkvert"))  {}
            if(ctrlWordData.ctrlWord.equals("trbgfdiag"))  {}
            if(ctrlWordData.ctrlWord.equals("trbghoriz"))  {}
            if(ctrlWordData.ctrlWord.equals("trbgvert"))  {}
            
            if(ctrlWordData.ctrlWord.equals("clFitText"))  {}
            if(ctrlWordData.ctrlWord.equals("clNoWrap"))  {}
            if(ctrlWordData.ctrlWord.equals("clpadl"))  {}
            if(ctrlWordData.ctrlWord.equals("clpadt"))  {}
            if(ctrlWordData.ctrlWord.equals("clpadb"))  {}
            if(ctrlWordData.ctrlWord.equals("clpadr"))  {}
            if(ctrlWordData.ctrlWord.equals("clpadfl"))  {}
            if(ctrlWordData.ctrlWord.equals("clpadft"))  {}
            if(ctrlWordData.ctrlWord.equals("clpadfb"))  {}
            if(ctrlWordData.ctrlWord.equals("clpadfr"))  {}
            if(ctrlWordData.ctrlWord.equals("clwWidth"))  {}
            if(ctrlWordData.ctrlWord.equals("clftsWidth"))  {}
            if(ctrlWordData.ctrlWord.equals("clhidemark"))  {}
            
            if(ctrlWordData.ctrlWord.equals("clins"))  {}
            if(ctrlWordData.ctrlWord.equals("cldel"))  {}
            if(ctrlWordData.ctrlWord.equals("clmrgd"))  {}
            if(ctrlWordData.ctrlWord.equals("clmrgdr"))  {}
            if(ctrlWordData.ctrlWord.equals("clsplit"))  {}
            if(ctrlWordData.ctrlWord.equals("clsplitr"))  {}
            if(ctrlWordData.ctrlWord.equals("clinsauth"))  {}
            if(ctrlWordData.ctrlWord.equals("clinsdttm"))  {}
            if(ctrlWordData.ctrlWord.equals("cldelauth"))  {}
            if(ctrlWordData.ctrlWord.equals("cldeldttm"))  {}
            if(ctrlWordData.ctrlWord.equals("clmrgdauth"))  {}
            if(ctrlWordData.ctrlWord.equals("clmrgddttm"))  {}
            
            if(ctrlWordData.ctrlWord.equals("tdfrmtxtLeft"))  {}
            if(ctrlWordData.ctrlWord.equals("tdfrmtxtRight"))  {}
            if(ctrlWordData.ctrlWord.equals("tdfrmtxtTop"))  {}
            if(ctrlWordData.ctrlWord.equals("tdfrmtxtBottom"))  {}
            if(ctrlWordData.ctrlWord.equals("tabsnoovrlp"))  {}
            if(ctrlWordData.ctrlWord.equals("tphcol"))  {}
            if(ctrlWordData.ctrlWord.equals("tphmrg"))  {}
            if(ctrlWordData.ctrlWord.equals("tphpg"))  {}
            if(ctrlWordData.ctrlWord.equals("tposnegx"))  {}
            if(ctrlWordData.ctrlWord.equals("tposnegy"))  {}
            if(ctrlWordData.ctrlWord.equals("tposx"))  {}
            if(ctrlWordData.ctrlWord.equals("tposxc"))  {}
            if(ctrlWordData.ctrlWord.equals("tposxi"))  {}
            if(ctrlWordData.ctrlWord.equals("tposxl"))  {}
            if(ctrlWordData.ctrlWord.equals("tposxo"))  {}
            if(ctrlWordData.ctrlWord.equals("tposxr"))  {}
            if(ctrlWordData.ctrlWord.equals("tposy"))  {}
            if(ctrlWordData.ctrlWord.equals("tposyb"))  {}
            if(ctrlWordData.ctrlWord.equals("tposyc"))  {}
            if(ctrlWordData.ctrlWord.equals("tposyil"))  {}
            if(ctrlWordData.ctrlWord.equals("tposyin"))  {}
            if(ctrlWordData.ctrlWord.equals("tposyout"))  {}
            if(ctrlWordData.ctrlWord.equals("tposyt"))  {}
            if(ctrlWordData.ctrlWord.equals("tpvmrg"))  {}
            if(ctrlWordData.ctrlWord.equals("tpvpara"))  {}
            if(ctrlWordData.ctrlWord.equals("tpvpg"))  {}
            
            if(ctrlWordData.ctrlWord.equals("rtlrow"))  {}
            if(ctrlWordData.ctrlWord.equals("ltrrow"))  {}
            
            if(ctrlWordData.ctrlWord.equals("trbrdrt"))  {}
            if(ctrlWordData.ctrlWord.equals("trbrdrl"))  {}
            if(ctrlWordData.ctrlWord.equals("trbrdrb"))  {}
            if(ctrlWordData.ctrlWord.equals("trbrdrr"))  {}
            if(ctrlWordData.ctrlWord.equals("trbrdrh"))  {}
            if(ctrlWordData.ctrlWord.equals("trbrdrv"))  {}
            
            if(ctrlWordData.ctrlWord.equals("brdrnil"))  {}
            if(ctrlWordData.ctrlWord.equals("clbrdrb"))  {}
            if(ctrlWordData.ctrlWord.equals("clbrdrt"))  {}
            if(ctrlWordData.ctrlWord.equals("clbrdrl"))  {}
            if(ctrlWordData.ctrlWord.equals("clbrdrr"))  {}
            if(ctrlWordData.ctrlWord.equals("cldglu"))  {}
            if(ctrlWordData.ctrlWord.equals("cldgll"))  {}
        }
        if(ctrlWordData.ctrlWordType == RtfCtrlWordType.TOGGLE) {
            this.rtfParser.getState().properties.toggleProperty(ctrlWordData);
        }
        
        if(ctrlWordData.ctrlWordType == RtfCtrlWordType.FLAG || 
                ctrlWordData.ctrlWordType == RtfCtrlWordType.VALUE) {
            this.rtfParser.getState().properties.setProperty(ctrlWordData);
        }
        
        switch(conversionType) {
        case RtfParser.TYPE_IMPORT_FULL:
            if(!IMPORT_IGNORED_CTRLWORDS.contains(ctrlWordData.ctrlWord)) {
                writeBuffer();
                writeText(ctrlWordData.toString());
            }
            result = true;
            break;        
        case RtfParser.TYPE_IMPORT_FRAGMENT:
            if(!IMPORT_IGNORED_CTRLWORDS.contains(ctrlWordData.ctrlWord)) {
                writeBuffer();
                writeText(ctrlWordData.toString());
            }
            result = true;
            break;
        case RtfParser.TYPE_CONVERT:
            if(!IMPORT_IGNORED_CTRLWORDS.contains(ctrlWordData.ctrlWord)) {
            }
            result = true;
            break;
        default:    
            result = false;
            break;
        }
        
        
        
        
        return result;
    }
    
    private void writeBuffer() {
        writeText(this.buffer.toString());
        setToDefaults();
    }
    
    private void writeText(String value) {
        if(this.rtfParser.isNewGroup()) {
            this.rtfDoc.add(new RtfDirectContent("{"));
            this.rtfParser.setNewGroup(false);
        }
        if(value.length() > 0) {
            this.rtfDoc.add(new RtfDirectContent(value));
        }
    }
    
    public void setToDefaults() {
        this.buffer = new StringBuffer(255);
    }
    
    public void afterPropertyChange(String propertyName) {
        if(propertyName.startsWith(RtfProperty.CHARACTER)) {
        } else {
            if(propertyName.startsWith(RtfProperty.PARAGRAPH)) {
            } else {
                if(propertyName.startsWith(RtfProperty.SECTION)) {
                } else {
                    if(propertyName.startsWith(RtfProperty.DOCUMENT)) {

                    }
                }
            }
        }        
    }
    
    
    public void beforePropertyChange(String propertyName) {
        
        
        if(this.buffer.length() == 0) return;
        
        if(propertyName.startsWith(RtfProperty.CHARACTER)) {
            
            
            Chunk chunk = new Chunk();
            chunk.append(this.buffer.toString());
            this.buffer = new StringBuffer(255);
            HashMap charProperties = this.rtfParser.getState().properties.getProperties(RtfProperty.CHARACTER);
            String defFont = (String)charProperties.get(RtfProperty.CHARACTER_FONT);
            if(defFont == null) defFont = "0";
            RtfDestinationFontTable fontTable = (RtfDestinationFontTable)this.rtfParser.getDestination("fonttbl");
            Font currFont = fontTable.getFont(defFont);
            int fs = Font.NORMAL;
            if(charProperties.containsKey(RtfProperty.CHARACTER_BOLD)) fs |= Font.BOLD; 
            if(charProperties.containsKey(RtfProperty.CHARACTER_ITALIC)) fs |= Font.ITALIC;
            if(charProperties.containsKey(RtfProperty.CHARACTER_UNDERLINE)) fs |= Font.UNDERLINE;
            Font useFont = FontFactory.getFont(currFont.getFamilyname(), 12, fs, new Color(0,0,0));
            
            
            chunk.setFont(useFont);
            if(iTextParagraph == null) this.iTextParagraph = new Paragraph();
            this.iTextParagraph.add(chunk);

        } else {
            if(propertyName.startsWith(RtfProperty.PARAGRAPH)) {
                
            } else {
                if(propertyName.startsWith(RtfProperty.SECTION)) {
                    
                } else {
                    if(propertyName.startsWith(RtfProperty.DOCUMENT)) {

                    }
                }
            }
        }        
    }
    
    private void addParagraphToDocument() {
        if(this.iTextParagraph != null) {
            try {
                this.rtfParser.getDocument().add(this.iTextParagraph);
            } catch (DocumentException e) {
                
                e.printStackTrace();
            }
            this.iTextParagraph = null;
        }    
    }
}
