

package com.lowagie.text.pdf.interfaces;

import com.lowagie.text.pdf.PdfAcroForm;
import com.lowagie.text.pdf.PdfAnnotation;
import com.lowagie.text.pdf.PdfFormField;

public interface PdfAnnotations {

    
    public PdfAcroForm getAcroForm();
    
    
    public void addAnnotation(PdfAnnotation annot);
    
    public void addCalculationOrder(PdfFormField annot);
    
    
    public void setSigFlags(int f);
}
