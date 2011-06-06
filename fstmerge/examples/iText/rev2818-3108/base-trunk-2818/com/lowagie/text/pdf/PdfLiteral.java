

package com.lowagie.text.pdf;



public class PdfLiteral extends PdfObject {
    
    
    private int position;
        
    public PdfLiteral(String text) {
        super(0, text);
    }
    
    public PdfLiteral(byte b[]) {
        super(0, b);
    }

    public PdfLiteral(int size) {
        super(0, (byte[])null);
        bytes = new byte[size];
        java.util.Arrays.fill(bytes, (byte)32);
    }

    public PdfLiteral(int type, String text) {
        super(type, text);
    }
    
    public PdfLiteral(int type, byte b[]) {
        super(type, b);
    }
    
    public void toPdf(PdfWriter writer, java.io.OutputStream os) throws java.io.IOException {
        if (os instanceof OutputStreamCounter)
            position = ((OutputStreamCounter)os).getCounter();
        super.toPdf(writer, os);
    }
    
    
    public int getPosition() {
        return this.position;
    }
    
    
    public int getPosLength() {
        if (bytes != null)
            return bytes.length;
        else
            return 0;
    }
    
}