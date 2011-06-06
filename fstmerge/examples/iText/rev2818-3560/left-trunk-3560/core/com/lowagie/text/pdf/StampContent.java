
package com.lowagie.text.pdf;

public class StampContent extends PdfContentByte {
    PdfStamperImp.PageStamp ps;
    PageResources pageResources;
    
    
    StampContent(PdfStamperImp stamper, PdfStamperImp.PageStamp ps) {
        super(stamper);
        this.ps = ps;
        pageResources = ps.pageResources;
    }
    
    public void setAction(PdfAction action, float llx, float lly, float urx, float ury) {
        ((PdfStamperImp)writer).addAnnotation(new PdfAnnotation(writer, llx, lly, urx, ury, action), ps.pageN);
    }

    
    public PdfContentByte getDuplicate() {
        return new StampContent((PdfStamperImp)writer, ps);
    }

    PageResources getPageResources() {
        return pageResources;
    }
    
    void addAnnotation(PdfAnnotation annot) {
        ((PdfStamperImp)writer).addAnnotation(annot, ps.pageN);
    }
}