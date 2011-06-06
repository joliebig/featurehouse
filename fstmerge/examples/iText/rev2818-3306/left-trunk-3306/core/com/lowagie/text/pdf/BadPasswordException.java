package com.lowagie.text.pdf;

import java.io.IOException;


public class BadPasswordException extends IOException {

    
    private static final long serialVersionUID = -4333706268155063964L;

    
    public BadPasswordException() {
        super("Bad user Password");
    }
}
