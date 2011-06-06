

package com.lowagie.text.rtf.field;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Font;
import com.lowagie.text.rtf.document.RtfDocument;



public class RtfPageNumber extends RtfField {

    
    private static final byte[] PAGE_NUMBER = "PAGE".getBytes();
    
    
    public RtfPageNumber() {
        super(null);
    }
    
    
    public RtfPageNumber(Font font) {
        super(null, font);
    }
    
    
    public RtfPageNumber(RtfDocument doc) {
        super(doc);
    }
    
    
    public RtfPageNumber(RtfDocument doc, Font font) {
        super(doc, font);
    }
    
    
    protected byte[] writeFieldInstContent() throws IOException 
    {
        return PAGE_NUMBER;
    }
    
    protected void writeFieldInstContent(final OutputStream out) throws IOException 
    {
        out.write(PAGE_NUMBER);
    }
    
    
    protected byte[] writeFieldResultContent() throws IOException 
    {
        return new byte[0];
    }
    
    protected void writeFieldResultContent(final OutputStream out) throws IOException 
    {        
    }
}
