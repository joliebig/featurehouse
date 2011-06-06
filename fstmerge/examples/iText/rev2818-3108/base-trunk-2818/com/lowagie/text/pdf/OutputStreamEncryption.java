
package com.lowagie.text.pdf;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.pdf.crypto.AESCipher;
import com.lowagie.text.pdf.crypto.IVGenerator;
import com.lowagie.text.pdf.crypto.ARCFOUREncryption;
import java.io.IOException;
import java.io.OutputStream;

public class OutputStreamEncryption extends OutputStream {
    
    protected OutputStream out;
    protected ARCFOUREncryption arcfour;
    protected AESCipher cipher;
    private byte[] sb = new byte[1];
    private static final int AES_128 = 4;
    private boolean aes;
    private boolean finished;
    
    
    public OutputStreamEncryption(OutputStream out, byte key[], int off, int len, int revision) {
        try {
            this.out = out;
            aes = revision == AES_128;
            if (aes) {
                byte[] iv = IVGenerator.getIV();
                byte[] nkey = new byte[len];
                System.arraycopy(key, off, nkey, 0, len);
                cipher = new AESCipher(true, nkey, iv);
                write(iv);
            }
            else {
                arcfour = new ARCFOUREncryption();
                arcfour.prepareARCFOURKey(key, off, len);
            }
        } catch (Exception ex) {
            throw new ExceptionConverter(ex);
        }
    }
    
    public OutputStreamEncryption(OutputStream out, byte key[], int revision) {
        this(out, key, 0, key.length, revision);
    }
    
    
    public void close() throws IOException {
        finish();
        out.close();
    }
    
    
    public void flush() throws IOException {
        out.flush();
    }
    
    
    public void write(byte[] b) throws IOException {
        write(b, 0, b.length);
    }
    
    
    public void write(int b) throws IOException {
        sb[0] = (byte)b;
        write(sb, 0, 1);
    }
    
    
    public void write(byte[] b, int off, int len) throws IOException {
        if (aes) {
            byte[] b2 = cipher.update(b, off, len);
            if (b2 == null || b2.length == 0)
                return;
            out.write(b2, 0, b2.length);
        }
        else {
            byte[] b2 = new byte[Math.min(len, 4192)];
            while (len > 0) {
                int sz = Math.min(len, b2.length);
                arcfour.encryptARCFOUR(b, off, sz, b2, 0);
                out.write(b2, 0, sz);
                len -= sz;
                off += sz;
            }
        }
    }
    
    public void finish() throws IOException {
        if (!finished) {
            finished = true;
            if (aes) {
                byte[] b;
                try {
                    b = cipher.doFinal();
                } catch (Exception ex) {
                    throw new ExceptionConverter(ex);
                }
                out.write(b, 0, b.length);
            }
        }
    }
}