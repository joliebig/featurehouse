

package com.lowagie.text.rtf.document.output;

import java.io.IOException;
import java.io.OutputStream;


public class RtfEfficientMemoryCache implements RtfDataCache 
{
    
    private final RtfByteArrayBuffer bab;
    
    
    public RtfEfficientMemoryCache()
    {
        bab = new RtfByteArrayBuffer();
    }
    
    
    public OutputStream getOutputStream()
    {
        return bab;
    }

    
    public void writeTo(OutputStream target) throws IOException 
    {
        bab.writeTo(target);
    }

}
