

package com.lowagie.text.rtf;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Reader;

import com.lowagie.text.DocWriter;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.HeaderFooter;
import com.lowagie.text.Rectangle;
import com.lowagie.text.rtf.direct.RtfImportMappings;
import com.lowagie.text.rtf.direct.RtfParser;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.document.RtfDocumentSettings;
import com.lowagie.text.rtf.text.RtfNewPage;


public class RtfWriter2 extends DocWriter {
    
    private RtfDocument rtfDoc = null;
    
    
    protected RtfWriter2(Document doc, OutputStream os) {
        super(doc, os);
        doc.addDocListener(this);
        rtfDoc = new RtfDocument();
    }

    
    public static RtfWriter2 getInstance(Document doc, OutputStream os) {
        return new RtfWriter2(doc, os);
    }

    
    public void setHeader(HeaderFooter hf) {
        this.rtfDoc.getDocumentHeader().setHeader(hf);
    }
    
    
    public void resetHeader() {
        this.rtfDoc.getDocumentHeader().setHeader(null);
    }
    
    
    public void setFooter(HeaderFooter hf) {
        this.rtfDoc.getDocumentHeader().setFooter(hf);
    }
    
    
    public void resetFooter() {
        this.rtfDoc.getDocumentHeader().setFooter(null);
    }

    
    public void setPageCount(int i) {}
    
    
    public void resetPageCount() {}

    
    public void clearTextWrap() {}

    
    public void open() {
        super.open();
        this.rtfDoc.open();
    }
    
    
    public void close() {
        if (open) {
            rtfDoc.writeDocument(os);
            super.close();
            this.rtfDoc = new RtfDocument();
        }
    }

    
    public boolean add(Element element) throws DocumentException {
        if (pause) {
            return false;
        }
        RtfBasicElement rtfElement = rtfDoc.getMapper().mapElement(element);
        if(rtfElement != null) {
            rtfDoc.add(rtfElement);
            return true;
        } else {
            return false;
        }
    }
    
    
    public boolean newPage() {
        rtfDoc.add(new RtfNewPage(rtfDoc));
        return true;
    }

    
    public boolean setMargins(float left, float right, float top, float bottom) {
        rtfDoc.getDocumentHeader().getPageSetting().setMarginLeft((int) (left * RtfElement.TWIPS_FACTOR));
        rtfDoc.getDocumentHeader().getPageSetting().setMarginRight((int) (right * RtfElement.TWIPS_FACTOR));
        rtfDoc.getDocumentHeader().getPageSetting().setMarginTop((int) (top * RtfElement.TWIPS_FACTOR));
        rtfDoc.getDocumentHeader().getPageSetting().setMarginBottom((int) (bottom * RtfElement.TWIPS_FACTOR));
        return true;
    }
    
    
    public boolean setPageSize(Rectangle rect) {
        rtfDoc.getDocumentHeader().getPageSetting().setPageSize(rect);
        return true;
    }
    
    
    public void setAutogenerateTOCEntries(boolean autogenerate) {
        this.rtfDoc.setAutogenerateTOCEntries(autogenerate);
    }
    
    
    public void setDataCacheStyle(int dataCacheStyle) {
        this.rtfDoc.getDocumentSettings().setDataCacheStyle(dataCacheStyle);
    }
    
    
    public RtfDocumentSettings getDocumentSettings() {
        return this.rtfDoc.getDocumentSettings();
    }
    
    
    public void importRtfDocument(Reader documentSource) throws IOException, DocumentException {
        if(!this.open) {
            throw new DocumentException("The document must be open to import RTF documents.");
        }
        RtfParser rtfImport = new RtfParser();
        rtfImport.importRtfDocument(documentSource, this.rtfDoc);
    }
    
    
    public void importRtfFragment(Reader documentSource, RtfImportMappings mappings) throws IOException, DocumentException {
        if(!this.open) {
            throw new DocumentException("The document must be open to import RTF fragments.");
        }
        RtfParser rtfImport = new RtfParser();
        rtfImport.importRtfFragment(documentSource, this.rtfDoc, mappings);
    }
}
