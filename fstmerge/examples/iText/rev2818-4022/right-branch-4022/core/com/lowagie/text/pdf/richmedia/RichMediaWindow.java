

package com.lowagie.text.pdf.richmedia;

import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;


public class RichMediaWindow extends PdfDictionary {

    
    public RichMediaWindow() {
        super(PdfName.RICHMEDIAWINDOW);
    }
    
    
    public void setWidth(float defaultWidth, float maxWidth, float minWidth) {
        put(PdfName.WIDTH, createDimensionDictionary(defaultWidth, maxWidth, minWidth));
    }

    
    public void setHeight(float defaultHeight, float maxHeight, float minHeight) {
        put(PdfName.HEIGHT, createDimensionDictionary(defaultHeight, maxHeight, minHeight));
    }
    
    
    private PdfDictionary createDimensionDictionary(float d, float max, float min) {
        PdfDictionary dict = new PdfDictionary();
        dict.put(PdfName.DEFAULT, new PdfNumber(d));
        dict.put(PdfName.MAX_CAMEL_CASE, new PdfNumber(max));
        dict.put(PdfName.MIN_CAMEL_CASE, new PdfNumber(min));
        return dict;
    }
    
    
    public void setPosition(RichMediaPosition position) {
        put(PdfName.POSITION, position);
    }
}
