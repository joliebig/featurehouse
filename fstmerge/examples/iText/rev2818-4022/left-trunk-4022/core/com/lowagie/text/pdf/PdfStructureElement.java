

package com.lowagie.text.pdf;


public class PdfStructureElement extends PdfDictionary {
    
    
    private PdfStructureElement parent;
    private PdfStructureTreeRoot top;
    
    
    private PdfIndirectReference reference;
    
    
    public PdfStructureElement(PdfStructureElement parent, PdfName structureType) {
        top = parent.top;
        init(parent, structureType);
        this.parent = parent;
        put(PdfName.P, parent.reference);
    }
    
        
    public PdfStructureElement(PdfStructureTreeRoot parent, PdfName structureType) {
        top = parent;
        init(parent, structureType);
        put(PdfName.P, parent.getReference());
    }
    
    private void init(PdfDictionary parent, PdfName structureType) {
        PdfObject kido = parent.get(PdfName.K);
        PdfArray kids = null;
        if (kido != null && !kido.isArray())
            throw new IllegalArgumentException("The parent has already another function.");
        if (kido == null) {
            kids = new PdfArray();
            parent.put(PdfName.K, kids);
        }
        else
            kids = (PdfArray)kido;
        kids.add(this);
        put(PdfName.S, structureType);
        reference = top.getWriter().getPdfIndirectReference();
    }
    
        
    public PdfDictionary getParent() {
        return parent;
    }
    
    void setPageMark(int page, int mark) {
        if (mark >= 0)
            put(PdfName.K, new PdfNumber(mark));
        top.setPageMark(page, reference);
    }
    
        
    public PdfIndirectReference getReference() {
        return this.reference;
    }
}
