

package com.lowagie.text.pdf.richmedia;

import com.lowagie.text.exceptions.IllegalPdfSyntaxException;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfIndirectReference;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfString;


public class CuePoint extends PdfDictionary {

    
    public CuePoint(PdfName subtype) {
        super(PdfName.CUEPOINT);
        put(PdfName.SUBTYPE, subtype);
    }
    
    
    public void setName(PdfString name) {
        put(PdfName.NAME, name);
    }
    
    
    public void setTime(int time) {
        put(PdfName.TIME, new PdfNumber(time));
    }

    
    public void setAction(PdfObject action) {
        if (action instanceof PdfDictionary || action instanceof PdfIndirectReference)
            put(PdfName.A, action);
        else
            throw new IllegalPdfSyntaxException(
                "An action should be defined as a dictionary");
    }
}
