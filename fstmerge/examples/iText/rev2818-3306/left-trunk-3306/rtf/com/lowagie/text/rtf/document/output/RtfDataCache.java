

package com.lowagie.text.rtf.document.output;

import java.io.IOException;
import java.io.OutputStream;



public interface RtfDataCache 
{
    
    public static final int CACHE_MEMORY_EFFICIENT = 3;
    
    public static final int CACHE_MEMORY = 2;
    
    public static final int CACHE_DISK = 1;
    
    
    public OutputStream getOutputStream();
    
    
    public void writeTo(OutputStream target) throws IOException;
}
