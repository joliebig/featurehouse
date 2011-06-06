

package com.lowagie.text.rtf.field;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Anchor;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.text.RtfPhrase;



public class RtfAnchor extends RtfField 
{
    
    private static final byte[] HYPERLINK = "HYPERLINK".getBytes();
    
    
    private String url = "";
    
    private RtfPhrase content = null;

    
    public RtfAnchor(RtfDocument doc, Anchor anchor) {
        super(doc);
        this.url = anchor.getReference();
        this.content = new RtfPhrase(doc, anchor);
    }
    
    
    protected void writeFieldInstContent(OutputStream result) throws IOException 
    {
        result.write(HYPERLINK);
        result.write(DELIMITER);
        this.document.filterSpecialChar(result, url, true, true);
    }
    
    
    protected void writeFieldResultContent(OutputStream result) throws IOException 
    {
        content.writeContent(result);
    }
    
}
