

package com.lowagie.text.exceptions;

import java.io.IOException;


public class InvalidPdfException extends IOException {

    
    private static final long serialVersionUID = -2319614911517026938L;

    
    public InvalidPdfException(String message) {
        super(message);
    }
}
