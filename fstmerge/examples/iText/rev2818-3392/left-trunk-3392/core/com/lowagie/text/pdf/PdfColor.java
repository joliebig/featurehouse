

package com.lowagie.text.pdf;
import java.awt.Color;


class PdfColor extends PdfArray {
    
    
    

    
    PdfColor(int red, int green, int blue) {
        super(new PdfNumber((double)(red & 0xFF) / 0xFF));
        add(new PdfNumber((double)(green & 0xFF) / 0xFF));
        add(new PdfNumber((double)(blue & 0xFF) / 0xFF));
    }
    
    PdfColor(Color color) {
        this(color.getRed(), color.getGreen(), color.getBlue());
    }
}