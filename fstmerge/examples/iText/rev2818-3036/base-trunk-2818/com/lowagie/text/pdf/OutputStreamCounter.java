
package com.lowagie.text.pdf;
import java.io.IOException;
import java.io.OutputStream;


public class OutputStreamCounter extends OutputStream {
    
    protected OutputStream out;
    protected int counter = 0;
    
    
    public OutputStreamCounter(OutputStream out) {
        this.out = out;
    }
    
    
    public void close() throws IOException {
        out.close();
    }
    
    
    public void flush() throws IOException {
        out.flush();
    }
    
    
    public void write(byte[] b) throws IOException {
        counter += b.length;
        out.write(b);
    }
    
    
    public void write(int b) throws IOException {
        ++counter;
        out.write(b);
    }
    
    
    public void write(byte[] b, int off, int len) throws IOException {
        counter += len;
        out.write(b, off, len);
    }
    
    public int getCounter() {
        return counter;
    }
    
    public void resetCounter() {
        counter = 0;
    }
}
