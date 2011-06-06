

package com.lowagie.text.rtf.document.output;

import java.io.OutputStream;


public final class RtfNilOutputStream extends OutputStream
{
    
    private long size = 0;
    
    
    public RtfNilOutputStream()
    {           
    }
    
    
    public long getSize()
    {
        return(size);
    }
    
    
    public void write(int b)
    {
        size++;
    }
    
    
    public void write(byte[] b, int off, int len)
    {
        if (b == null) {
            throw new NullPointerException();
        } else if ((off < 0) || (off > b.length) || (len < 0) ||
               ((off + len) > b.length) || ((off + len) < 0)) {
            throw new IndexOutOfBoundsException();
        }
        size += len;
    }
}