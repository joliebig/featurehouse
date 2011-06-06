

package com.lowagie.text.pdf;



public class PdfBorderArray extends PdfArray {
    
    
    

    
    public PdfBorderArray(float hRadius, float vRadius, float width) {
        this(hRadius, vRadius, width, null);
    }
    

    
    public PdfBorderArray(float hRadius, float vRadius, float width, PdfDashPattern dash) {
        super(new PdfNumber(hRadius));
        add(new PdfNumber(vRadius));
        add(new PdfNumber(width));
        if (dash != null)
            add(dash);
    }
}