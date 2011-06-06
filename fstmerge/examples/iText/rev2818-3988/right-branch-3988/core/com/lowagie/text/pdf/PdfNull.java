

package com.lowagie.text.pdf;



public class PdfNull extends PdfObject {
    
    
    
    
    public static final PdfNull    PDFNULL = new PdfNull();
    
    
    private static final String CONTENT = "null";
    
    
    
    
    public PdfNull() {
        super(NULL, CONTENT);
    }
    
    
    
    public String toString() {
        return "null";
    }
}