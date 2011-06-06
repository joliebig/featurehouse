

package com.lowagie.text.rtf.document;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.HeaderFooter;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.document.output.RtfNilOutputStream;
import com.lowagie.text.rtf.headerfooter.RtfHeaderFooter;
import com.lowagie.text.rtf.headerfooter.RtfHeaderFooterGroup;
import com.lowagie.text.rtf.list.RtfList;
import com.lowagie.text.rtf.list.RtfListTable;
import com.lowagie.text.rtf.style.RtfColor;
import com.lowagie.text.rtf.style.RtfColorList;
import com.lowagie.text.rtf.style.RtfFont;
import com.lowagie.text.rtf.style.RtfFontList;
import com.lowagie.text.rtf.style.RtfParagraphStyle;
import com.lowagie.text.rtf.style.RtfStylesheetList;



public class RtfDocumentHeader extends RtfElement {
    
    private static final byte[] TITLE_PAGE = "\\titlepg".getBytes();
    
    private static final byte[] FACING_PAGES = "\\facingp".getBytes();
    
    
    private RtfCodePage codePage = null;
    
    private RtfColorList colorList = null;
    
    private RtfFontList fontList = null;
    
    private RtfListTable listTable = null;
    
    private RtfStylesheetList stylesheetList = null;
    
    private RtfInfoGroup infoGroup = null;
    
    private RtfPageSetting pageSetting = null;
    
    private HeaderFooter header = null;
    
    private HeaderFooter footer = null;

    
    protected RtfDocumentHeader(RtfDocument doc) {
        super(doc);
    }

    
    protected void init() {
        this.codePage = new RtfCodePage(this.document);
        this.colorList = new RtfColorList(this.document);
        this.fontList = new RtfFontList(this.document);
        this.listTable = new RtfListTable(this.document);
        this.stylesheetList = new RtfStylesheetList(this.document);
        this.infoGroup = new RtfInfoGroup(this.document);
        this.pageSetting = new RtfPageSetting(this.document);
        this.header = new RtfHeaderFooterGroup(this.document, RtfHeaderFooter.TYPE_HEADER);
        this.footer = new RtfHeaderFooterGroup(this.document, RtfHeaderFooter.TYPE_FOOTER);
    }
    
    
    public byte[] write() {        
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
        try {
            
            
            
            writeSectionDefinition(new RtfNilOutputStream());
            








            this.codePage.writeDefinition(result);
            this.fontList.writeDefinition(result);
            this.colorList.writeDefinition(result);
            this.stylesheetList.writeDefinition(result);
            this.listTable.writeDefinition(result);
            this.infoGroup.writeContent(result);
            this.pageSetting.writeDefinition(result);
            
            
            writeSectionDefinition(result);
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
    }        

    
    public byte[] writeSectionDefinition()
    {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
           writeSectionDefinition(result);
        return result.toByteArray();
    }
    
    public void writeSectionDefinition(final OutputStream result) 
    {
        try {
            RtfHeaderFooterGroup header = convertHeaderFooter(this.header, RtfHeaderFooter.TYPE_HEADER);
            RtfHeaderFooterGroup footer = convertHeaderFooter(this.footer, RtfHeaderFooter.TYPE_FOOTER);
            if(header.hasTitlePage() || footer.hasTitlePage()) {
                result.write(TITLE_PAGE);
                header.setHasTitlePage();
                footer.setHasTitlePage();
            }
            if(header.hasFacingPages() || footer.hasFacingPages()) {
                result.write(FACING_PAGES);
                header.setHasFacingPages();
                footer.setHasFacingPages();
            }
            
            footer.writeContent(result);
            
            header.writeContent(result);
            
            pageSetting.writeSectionDefinition(result);
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }        
    }
    
    
    public int getFontNumber(RtfFont font) {
        return this.fontList.getFontNumber(font);
    }

    
    public int getColorNumber(RtfColor color) {
        return this.colorList.getColorNumber(color);
    }
    
    
    public int getListNumber(RtfList list) {
        return this.listTable.getListNumber(list);
    }
    
    
    public RtfParagraphStyle getRtfParagraphStyle(String styleName) {
        return this.stylesheetList.getRtfParagraphStyle(styleName);
    }
    
    
    public void freeListNumber(RtfList list) {
        this.listTable.freeListNumber(list);
    }
    
    
    public RtfPageSetting getPageSetting() {
        return this.pageSetting;
    }
    
    
    public void addInfoElement(RtfInfoElement rtfInfoElement) {
        this.infoGroup.add(rtfInfoElement);
    }
    
    
    public void setHeader(HeaderFooter header) {
        this.header = header;
    }
    
    
    public void setFooter(HeaderFooter footer) {
        this.footer = footer;
    }
    
    
    public void registerParagraphStyle(RtfParagraphStyle rtfParagraphStyle) {
        this.stylesheetList.registerParagraphStyle(rtfParagraphStyle);
    }
    
    
    private RtfHeaderFooterGroup convertHeaderFooter(HeaderFooter hf, int type) {
        if(hf != null) {
            if(hf instanceof RtfHeaderFooterGroup) {
                return new RtfHeaderFooterGroup(this.document, (RtfHeaderFooterGroup) hf, type);
            } else if(hf instanceof RtfHeaderFooter) {
                return new RtfHeaderFooterGroup(this.document, (RtfHeaderFooter) hf, type);
            } else {
                return new RtfHeaderFooterGroup(this.document, hf, type);
            }
        } else {
            return new RtfHeaderFooterGroup(this.document, type);
        }
    }
}
