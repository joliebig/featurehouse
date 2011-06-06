
package com.lowagie.text.rtf.direct;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.rtf.RtfAddableElement;


public class RtfDirectContent extends RtfAddableElement {
    
    public static final RtfDirectContent DIRECT_SOFT_LINEBREAK = new RtfDirectContent("\\line");
    
    
    private String directContent = "";
    
    
    public RtfDirectContent(String directContent)
    {
        this.directContent = directContent;
    }
    
    
    public byte[] write() 
    {
        return this.directContent.getBytes();
    }
    
        
    public void writeContent(final OutputStream out) throws IOException
    {
        final byte[] contentBytes = this.directContent.getBytes();
           out.write(contentBytes);
    }        
    
    
}
