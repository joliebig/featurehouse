
package com.lowagie.text.pdf;
import java.awt.Color;

import com.lowagie.text.Rectangle;


public class BarcodeEANSUPP extends Barcode{
    
        
    protected Barcode ean;
        
    protected Barcode supp;
    
    
    public BarcodeEANSUPP(Barcode ean, Barcode supp) {
        n = 8; 
        this.ean = ean;
        this.supp = supp;
    }
    
    
    public Rectangle getBarcodeSize() {
        Rectangle rect = ean.getBarcodeSize();
        rect.setRight(rect.getWidth() + supp.getBarcodeSize().getWidth() + n);
        return rect;
    }
    
    
    public Rectangle placeBarcode(PdfContentByte cb, Color barColor, Color textColor) {
        if (supp.getFont() != null)
            supp.setBarHeight(ean.getBarHeight() + supp.getBaseline() - supp.getFont().getFontDescriptor(BaseFont.CAPHEIGHT, supp.getSize()));
        else
            supp.setBarHeight(ean.getBarHeight());
        Rectangle eanR = ean.getBarcodeSize();
        cb.saveState();
        ean.placeBarcode(cb, barColor, textColor);
        cb.restoreState();
        cb.saveState();
        cb.concatCTM(1, 0, 0, 1, eanR.getWidth() + n, eanR.getHeight() - ean.getBarHeight());
        supp.placeBarcode(cb, barColor, textColor);
        cb.restoreState();
        return getBarcodeSize();
    }
    
        
    public java.awt.Image createAwtImage(Color foreground, Color background) {
        throw new UnsupportedOperationException("The two barcodes must be composed externally.");
    }    
}
