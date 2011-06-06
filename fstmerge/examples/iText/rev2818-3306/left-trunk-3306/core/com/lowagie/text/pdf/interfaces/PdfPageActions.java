

package com.lowagie.text.pdf.interfaces;

import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.PdfAction;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfTransition;



public interface PdfPageActions {
    
        
    public void setPageAction(PdfName actionType, PdfAction action) throws DocumentException;

    
    public void setDuration(int seconds);
    
    
    public void setTransition(PdfTransition transition);
}
