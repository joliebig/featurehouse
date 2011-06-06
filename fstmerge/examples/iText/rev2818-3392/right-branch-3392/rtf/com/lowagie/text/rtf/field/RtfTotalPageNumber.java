

package com.lowagie.text.rtf.field;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Font;
import com.lowagie.text.rtf.document.RtfDocument;


public class RtfTotalPageNumber extends RtfField {

    
    private static final byte[] ARABIC_TOTAL_PAGES = "NUMPAGES \\\\* Arabic".getBytes();
    
    
    public RtfTotalPageNumber() {
        super(null);
    }
    
    
    public RtfTotalPageNumber(Font font) {
        super(null, font);
    }
    
    
    public RtfTotalPageNumber(RtfDocument doc) {
        super(doc);
    }
    
    
    public RtfTotalPageNumber(RtfDocument doc, Font font) {
        super(doc, font);
    }
    
     
    protected void writeFieldInstContent(OutputStream result) throws IOException 
    {
        result.write(ARABIC_TOTAL_PAGES);
    }

    
    protected void writeFieldResultContent(final OutputStream out) throws IOException 
    {
        out.write("1".getBytes());
    }
}
