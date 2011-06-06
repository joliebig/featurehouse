

package com.lowagie.text.rtf.text;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.document.RtfDocument;



public class RtfNewPage extends RtfElement {

    
    public static final byte[] NEW_PAGE = "\\page".getBytes();
    
    
    public RtfNewPage(RtfDocument doc) {
        super(doc);
    }
    
     
    public void writeContent(final OutputStream result) throws IOException
    {
        result.write(NEW_PAGE);
        result.write(RtfParagraph.PARAGRAPH_DEFAULTS);        
    }
    
}
