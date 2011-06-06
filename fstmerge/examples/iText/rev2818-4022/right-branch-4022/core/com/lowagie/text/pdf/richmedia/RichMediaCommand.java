

package com.lowagie.text.pdf.richmedia;

import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfString;


public class RichMediaCommand extends PdfDictionary {
    
    
    public RichMediaCommand(PdfString command) {
        super(PdfName.RICHMEDIACOMMAND);
        put(PdfName.C, command);
    }
    
    
    public void setArguments(PdfObject args) {
        put(PdfName.A, args);
    }
}
