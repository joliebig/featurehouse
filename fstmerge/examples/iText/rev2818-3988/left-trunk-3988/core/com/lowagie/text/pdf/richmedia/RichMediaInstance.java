

package com.lowagie.text.pdf.richmedia;

import com.lowagie.text.exceptions.IllegalPdfSyntaxException;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfIndirectReference;
import com.lowagie.text.pdf.PdfName;


public class RichMediaInstance extends PdfDictionary {

    
    protected boolean flash;
    
    
    public RichMediaInstance(PdfName subtype) {
        super(PdfName.RICHMEDIAINSTANCE);
        put(PdfName.SUBTYPE, subtype);
        flash = PdfName.FLASH.equals(subtype);
    }
    
    
    public void setParams(RichMediaParams params) {
        if (flash) {
            put(PdfName.PARAMS, params);
        }
        else {
            throw new IllegalPdfSyntaxException("Parameters can only be set for Flash instances.");
        }
    }
    
    
    public void setAsset(PdfIndirectReference asset) {
        put(PdfName.ASSET, asset);
    }
}
