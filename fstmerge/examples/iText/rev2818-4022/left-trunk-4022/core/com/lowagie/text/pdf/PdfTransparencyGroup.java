
package com.lowagie.text.pdf;


public class PdfTransparencyGroup extends PdfDictionary {
    
    
    public PdfTransparencyGroup() {
        super();
        put(PdfName.S, PdfName.TRANSPARENCY);
    }
 
    
    public void setIsolated(boolean isolated) {
        if (isolated)
            put(PdfName.I, PdfBoolean.PDFTRUE);
        else
            remove(PdfName.I);
    }
    
    
    public void setKnockout(boolean knockout) {
        if (knockout)
            put(PdfName.K, PdfBoolean.PDFTRUE);
        else
            remove(PdfName.K);
    }

}
