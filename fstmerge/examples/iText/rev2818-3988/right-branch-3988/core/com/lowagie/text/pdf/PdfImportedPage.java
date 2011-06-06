

package com.lowagie.text.pdf;
import java.io.IOException;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Image;


public class PdfImportedPage extends com.lowagie.text.pdf.PdfTemplate {

    PdfReaderInstance readerInstance;
    int pageNumber;
    
    PdfImportedPage(PdfReaderInstance readerInstance, PdfWriter writer, int pageNumber) {
        this.readerInstance = readerInstance;
        this.pageNumber = pageNumber;
        thisReference = writer.getPdfIndirectReference();
        bBox = readerInstance.getReader().getPageSize(pageNumber);
        setMatrix(1, 0, 0, 1, -bBox.getLeft(), -bBox.getBottom());
        type = TYPE_IMPORTED;
    }

    
    public PdfImportedPage getFromReader() {
      return this;
    }

    public int getPageNumber() {
        return pageNumber;
    }


        
    public void addImage(Image image, float a, float b, float c, float d, float e, float f) throws DocumentException {
        throwError();
    }
    
        
    public void addTemplate(PdfTemplate template, float a, float b, float c, float d, float e, float f) {
        throwError();
    }
    
        
    public PdfContentByte getDuplicate() {
        throwError();
        return null;
    }

    
    PdfStream getFormXObject(int compressionLevel) throws IOException {
         return readerInstance.getFormXObject(pageNumber, compressionLevel);
    }
    
    public void setColorFill(PdfSpotColor sp, float tint) {
        throwError();
    }
    
    public void setColorStroke(PdfSpotColor sp, float tint) {
        throwError();
    }
    
    PdfObject getResources() {
        return readerInstance.getResources(pageNumber);
    }
    
        
    public void setFontAndSize(BaseFont bf, float size) {
        throwError();
    }
    
    void throwError() {
        throw new RuntimeException("Content can not be added to a PdfImportedPage.");
    }
    
    PdfReaderInstance getPdfReaderInstance() {
        return readerInstance;
    }
}
