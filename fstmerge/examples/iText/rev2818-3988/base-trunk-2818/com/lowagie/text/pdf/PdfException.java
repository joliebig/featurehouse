

package com.lowagie.text.pdf;

import com.lowagie.text.DocumentException;



public class PdfException extends DocumentException {
    
    
    
    private static final long serialVersionUID = 6767433960955483999L;

    public PdfException(Exception ex) {
        super(ex);
    }

 
    
    PdfException() {
        super();
    }
    

    
    PdfException(String message) {
        super(message);
    }
}