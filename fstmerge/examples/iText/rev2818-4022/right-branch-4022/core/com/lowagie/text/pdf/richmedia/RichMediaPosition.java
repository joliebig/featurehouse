

package com.lowagie.text.pdf.richmedia;

import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;


public class RichMediaPosition extends PdfDictionary {

    
    public RichMediaPosition() {
        super(PdfName.RICHMEDIAPOSITION);
    }
    
    
    public void setHAlign(PdfName hAlign) {
        put(PdfName.HALIGN, hAlign);
    }
    
    
    public void setVAlign(PdfName vAlign) {
        put(PdfName.VALIGN, vAlign);
    }
    
    
    public void setHOffset(float hOffset) {
        put(PdfName.HOFFSET, new PdfNumber(hOffset));
    }
    
    
    public void setVOffset(float vOffset) {
        put(PdfName.VOFFSET, new PdfNumber(vOffset));
    }
}
