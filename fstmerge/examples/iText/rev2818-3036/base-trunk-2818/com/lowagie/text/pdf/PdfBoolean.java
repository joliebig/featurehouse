

package com.lowagie.text.pdf;



public class PdfBoolean extends PdfObject {
    
    
    public static final PdfBoolean PDFTRUE = new PdfBoolean(true);
    public static final PdfBoolean PDFFALSE = new PdfBoolean(false);

    public static final String TRUE = "true";
    

    public static final String FALSE = "false";
    
    
    

    private boolean value;
    
    
    

    
    public PdfBoolean(boolean value) {
        super(BOOLEAN);
        if (value) {
            setContent(TRUE);
        }
        else {
            setContent(FALSE);
        }
        this.value = value;
    }
    

    
    public PdfBoolean(String value) throws BadPdfFormatException {
        super(BOOLEAN, value);
        if (value.equals(TRUE)) {
            this.value = true;
        }
        else if (value.equals(FALSE)) {
            this.value = false;
        }
        else {
            throw new BadPdfFormatException("The value has to be 'true' of 'false', instead of '" + value + "'.");
        }
    }
    
    
    

    
    public boolean booleanValue() {
        return value;
    }
    
    public String toString() {
        return value ? TRUE : FALSE;
    }
}