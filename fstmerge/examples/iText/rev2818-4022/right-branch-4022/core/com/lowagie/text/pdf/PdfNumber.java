

package com.lowagie.text.pdf;


public class PdfNumber extends PdfObject {

    
    
    
    private double value;
    
    
    
    
    public PdfNumber(String content) {
        super(NUMBER);
        try {
            value = Double.parseDouble(content.trim());
            setContent(content);
        }
        catch (NumberFormatException nfe){
            throw new RuntimeException(content + " is not a valid number - " + nfe.toString());
        }
    }
    
    
    public PdfNumber(int value) {
        super(NUMBER);
        this.value = value;
        setContent(String.valueOf(value));
    }
    
    
    public PdfNumber(double value) {
        super(NUMBER);
        this.value = value;
        setContent(ByteBuffer.formatDouble(value));
    }
    
    
    public PdfNumber(float value) {
        this((double)value);
    }
    
    
    
    
    public int intValue() {
        return (int) value;
    }
    
    
    public double doubleValue() {
        return value;
    }
    
    
    public float floatValue() {
        return (float)value;
    }
    
    
    
    
    public void increment() {
        value += 1.0;
        setContent(ByteBuffer.formatDouble(value));
    }
}