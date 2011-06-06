

package com.lowagie.text.pdf.draw;

import com.lowagie.text.pdf.PdfContentByte;


public interface DrawInterface {
    
    public void draw(PdfContentByte canvas, float llx, float lly, float urx, float ury, float y);    
}