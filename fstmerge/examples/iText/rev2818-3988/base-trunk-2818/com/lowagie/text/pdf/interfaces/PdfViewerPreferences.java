

package com.lowagie.text.pdf.interfaces;

import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfObject;



public interface PdfViewerPreferences {    
        
    
    public void setViewerPreferences(int preferences);
    
    
    public void addViewerPreference(PdfName key, PdfObject value);        
}