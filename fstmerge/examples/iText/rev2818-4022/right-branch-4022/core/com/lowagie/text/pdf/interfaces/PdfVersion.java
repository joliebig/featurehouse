

package com.lowagie.text.pdf.interfaces;

import com.lowagie.text.pdf.PdfDeveloperExtension;
import com.lowagie.text.pdf.PdfName;



public interface PdfVersion {
    
    
    public void setPdfVersion(char version);
    
    public void setAtLeastPdfVersion(char version);
    
    public void setPdfVersion(PdfName version);
    
    public void addDeveloperExtension(PdfDeveloperExtension de);
}