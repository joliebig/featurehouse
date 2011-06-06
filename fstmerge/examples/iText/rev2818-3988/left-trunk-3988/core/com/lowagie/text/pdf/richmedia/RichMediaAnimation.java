

package com.lowagie.text.pdf.richmedia;

import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;


public class RichMediaAnimation extends PdfDictionary {
    
    
    public RichMediaAnimation(PdfName subtype) {
        super(PdfName.RICHMEDIAANIMATION);
        put(PdfName.SUBTYPE, subtype);
    }
    
    
    public void setPlayCount(int playCount) {
        put(PdfName.PLAYCOUNT, new PdfNumber(playCount));
    }
    
    
    public void setSpeed(int speed) {
        put(PdfName.SPEED, new PdfNumber(speed));
    }
    
    
    public void setSpeed(float speed) {
        put(PdfName.SPEED, new PdfNumber(speed));
    }
}
