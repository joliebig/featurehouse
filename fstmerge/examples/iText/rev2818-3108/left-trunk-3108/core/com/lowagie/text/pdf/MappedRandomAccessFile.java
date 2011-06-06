
package com.lowagie.text.pdf;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.BufferUnderflowException;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.security.AccessController;
import java.security.PrivilegedAction;


public class MappedRandomAccessFile {
    
    private MappedByteBuffer mappedByteBuffer = null;
    private FileChannel channel = null;
    
    
    public MappedRandomAccessFile(String filename, String mode)
    throws FileNotFoundException, IOException {
        
        if (mode.equals("rw"))
            init(
                    new java.io.RandomAccessFile(filename, mode).getChannel(),
                    FileChannel.MapMode.READ_WRITE);
        else
            init(
                    new FileInputStream(filename).getChannel(),
                    FileChannel.MapMode.READ_ONLY);
        
    }
    
    
    private void init(FileChannel channel, FileChannel.MapMode mapMode)
    throws IOException {
        
        this.channel = channel;
        this.mappedByteBuffer = channel.map(mapMode, 0L, channel.size());
        mappedByteBuffer.load();
    }

    
    public FileChannel getChannel() {
        return channel;
    }
    
    
    public int read() {
        try {
            byte b = mappedByteBuffer.get();
            int n = b & 0xff;
            
            return n;
        } catch (BufferUnderflowException e) {
            return -1; 
        }
    }
    
    
    public int read(byte bytes[], int off, int len) {
        int pos = mappedByteBuffer.position();
        int limit = mappedByteBuffer.limit();
        if (pos == limit)
            return -1; 
        int newlimit = pos + len - off;
        if (newlimit > limit) {
            len = limit - pos; 
        }
        mappedByteBuffer.get(bytes, off, len);
        return len;
    }
    
    
    public long getFilePointer() {
        return mappedByteBuffer.position();
    }
    
    
    public void seek(long pos) {
        mappedByteBuffer.position((int) pos);
    }
    
    
    public long length() {
        return mappedByteBuffer.limit();
    }
    
    
    public void close() throws IOException {
        clean(mappedByteBuffer);
        mappedByteBuffer = null;
        if (channel != null)
            channel.close();
        channel = null;
    }
    
    
    protected void finalize() throws Throwable {
        close();
        super.finalize();
    }
    
    
    public static boolean clean(final java.nio.ByteBuffer buffer) {
        if (buffer == null || !buffer.isDirect())
            return false;
        
        Boolean b = (Boolean) AccessController.doPrivileged(new PrivilegedAction() {
            public Object run() {
                Boolean success = Boolean.FALSE;
                try {
                    Method getCleanerMethod = buffer.getClass().getMethod("cleaner", null);
                    getCleanerMethod.setAccessible(true);
                    Object cleaner = getCleanerMethod.invoke(buffer, null);
                    Method clean = cleaner.getClass().getMethod("clean", null);
                    clean.invoke(cleaner, null);
                    success = Boolean.TRUE;
                } catch (Exception e) {
                    
                    
                }
                return success;
            }
        });
        
        return b.booleanValue();
    }
    
}
