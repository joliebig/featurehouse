

package com.lowagie.text.rtf.field;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.DocWriter;
import com.lowagie.text.Font;
import com.lowagie.text.rtf.document.RtfDocument;



public class RtfPageNumber extends RtfField {

    
    private static final byte[] PAGE_NUMBER = DocWriter.getISOBytes("PAGE");
    
    
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
    
    
    protected void writeFieldInstContent(final OutputStream result) throws IOException 
    {
        result.write(PAGE_NUMBER);
    }
    
    
    protected void writeFieldResultContent(final OutputStream result) throws IOException {        
    }
}
