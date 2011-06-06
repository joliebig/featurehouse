

package com.lowagie.text.rtf.document.output;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;


public class RtfMemoryCache implements RtfDataCache {

    
    private ByteArrayOutputStream data = null;
    
    
    public RtfMemoryCache() {
        this.data = new ByteArrayOutputStream();
    }
    
    
    public OutputStream getOutputStream() {
        return this.data;
    }

    
    public void writeTo(OutputStream target) throws IOException {
        this.data.writeTo(target);
    }

}
