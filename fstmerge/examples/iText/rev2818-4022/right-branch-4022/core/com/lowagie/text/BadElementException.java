

package com.lowagie.text;



public class BadElementException extends DocumentException {
    
    private static final long serialVersionUID = -799006030723822254L;

    
    
    public BadElementException(Exception ex) {
        super(ex);
    }
    
    
    BadElementException() {
        super();
    }
    
    
    public BadElementException(String message) {
        super(message);
    }
}