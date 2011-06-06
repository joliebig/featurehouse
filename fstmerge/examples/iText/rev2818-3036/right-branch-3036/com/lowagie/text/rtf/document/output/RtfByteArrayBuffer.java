

package com.lowagie.text.rtf.document.output;

import java.io.*;
import java.util.ArrayList;
import java.util.List;


public final class RtfByteArrayBuffer extends OutputStream
{
    private final List<byte[]> arrays = new ArrayList<byte[]>();
    private byte[] buffer;
    private int pos = 0;
    private int size = 0;
    
    
    public RtfByteArrayBuffer()
    {            
        this(256);
    }
    
    public RtfByteArrayBuffer(final int bufferSize)
    {
        if((bufferSize <= 0) || (bufferSize > 1<<30)) throw(new IllegalArgumentException("bufferSize "+bufferSize));
        
        int n = 1<<5;
        while(n < bufferSize) {
            n <<= 1;
        }
        buffer = new byte[n];
    }
    
    public String toString()
    {
        return("RtfByteArrayBuffer: size="+size()+" #arrays="+arrays.size()+" pos="+pos);
    }
    
    
    public void reset()
    {
        arrays.clear();
        pos = 0;
        size = 0;
    }
    
    
    public long size()
    {
        return(size);
    }
    
    private void flushBuffer()
    {
        flushBuffer(1);
    }
    private void flushBuffer(final int reqSize)
    {
        if(reqSize < 0) throw(new IllegalArgumentException());
        
        if(pos == 0) return;

        if(pos == buffer.length) {
            
            arrays.add(buffer);
            int newSize = buffer.length;
            buffer = null;
            final int MAX = Math.max(1, size>>24) << 16;
            while(newSize < MAX) {
                newSize <<= 1;
                if(newSize >= reqSize) break;
            }
            buffer = new byte[newSize];
        } else {
            
            final byte[] c = new byte[pos];
            System.arraycopy(buffer, 0, c, 0, pos);
            arrays.add(c);                
        }
        pos = 0;            
    }
    
    
    public void write(final int b)
    {
        buffer[pos] = (byte)b;
        size++;
        if(++pos == buffer.length) flushBuffer();
    }        
    
    public void write(final byte[] src)
    {
        if(src == null) throw(new NullPointerException());

        if(src.length < buffer.length - pos) {
            System.arraycopy(src, 0, buffer, pos, src.length);
            pos += src.length;
            size += src.length;
            return;
        }
        writeLoop(src, 0, src.length);
    }
    
    public void write(final byte[] src, int off, int len)
    {
        if(src == null) throw(new NullPointerException());
        if((off < 0) || (off > src.length) || (len < 0) || ((off + len) > src.length) || ((off + len) < 0)) throw new IndexOutOfBoundsException();

        writeLoop(src, off, len);        
    }
    private void writeLoop(final byte[] src, int off, int len)
    {
        while(len > 0) {
            final int room = buffer.length - pos;
            final int n = len > room ? room : len;
            System.arraycopy(src, off, buffer, pos, n);
            len -= n;
            off += n;
            pos += n;
            size += n;
            if(pos == buffer.length) flushBuffer(len);
        }        
    }
    
    
    public long write(final InputStream in) throws IOException
    {
        if(in == null) throw(new NullPointerException());
        
        final long sizeStart = size;
        while(true) {
            final int n = in.read(buffer, pos, buffer.length - pos);
            if(n < 0) break;
            pos += n;
            size += n;
            if(pos == buffer.length) flushBuffer();
        }
        return(size - sizeStart);
    }
    
    
    public void append(final byte[] a)
    {
        if(a == null) throw(new NullPointerException());
        if(a.length == 0) return;
        
        if(a.length <= 8) {
            write(a, 0, a.length);        
        } else
        if((a.length <= 16) && (pos > 0) && ((buffer.length - pos) > a.length)) {
            write(a, 0, a.length);
        } else {
            flushBuffer();
            arrays.add(a);
            size += a.length;
        }
    }
    
    public void append(final byte[][] a)
    {
        if(a == null) throw(new NullPointerException());

        for(int k = 0; k < a.length; k++) {
            append(a[k]);
        }
    }
    
    
    public byte[][] toByteArrayArray()
    {
        flushBuffer();
        return arrays.toArray(new byte[arrays.size()][]);
    }
    
    
    public byte[] toByteArray()
    {
        final byte[] r = new byte[size];
        int off = 0;
        final int n = arrays.size();
        for(int k = 0; k < n; k++) {
            byte[] src = arrays.get(k);
            System.arraycopy(src, 0, r, off, src.length);
            off += src.length;
        }
        if(pos > 0) System.arraycopy(buffer, 0, r, off, pos);
        return(r);
    }

    
    public void writeTo(final OutputStream out) throws IOException
    {
        if(out == null) throw(new NullPointerException());
        
        final int n = arrays.size();
        for(int k = 0; k < n; k++) {
            byte[] src = arrays.get(k);
            out.write(src);
        }
        if(pos > 0) out.write(buffer, 0, pos);
    }
}
