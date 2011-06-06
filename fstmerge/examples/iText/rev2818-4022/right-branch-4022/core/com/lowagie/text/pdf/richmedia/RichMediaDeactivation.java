

package com.lowagie.text.pdf.richmedia;

import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;


public class RichMediaDeactivation extends PdfDictionary {
    
    
    public RichMediaDeactivation() {
        super(PdfName.RICHMEDIADEACTIVATION);
    }
    
    
    public void setCondition(PdfName condition) {
        put(PdfName.CONDITION, condition);
    }
}
