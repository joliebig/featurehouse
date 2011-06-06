

package com.lowagie.text.rtf.text;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.document.RtfDocument;



public class RtfNewPage extends RtfElement {

    
    public static final byte[] NEW_PAGE = "\\page".getBytes();
    
    
    public RtfNewPage(RtfDocument doc) {
        super(doc);
    }
    
    
    public byte[] write() {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            writeContent(result);
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }
     
    public void writeContent(final OutputStream result) throws IOException
    {
        result.write(NEW_PAGE);
        result.write(RtfParagraph.PARAGRAPH_DEFAULTS);        
    }
    
}
