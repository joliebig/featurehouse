

package com.lowagie.text.pdf;

import com.lowagie.text.Document;
import java.io.ByteArrayOutputStream;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.net.URL;

public class RandomAccessFileOrArray implements DataInput {
    
    MappedRandomAccessFile rf;
    RandomAccessFile trf;
    boolean plainRandomAccess;
    String filename;
    byte arrayIn[];
    int arrayInPtr;
    byte back;
    boolean isBack = false;
    
    
    private int startOffset = 0;

    public RandomAccessFileOrArray(String filename) throws IOException {
        this(filename, false, Document.plainRandomAccess);
    }
    
    public RandomAccessFileOrArray(String filename, boolean forceRead, boolean plainRandomAccess) throws IOException {
        this.plainRandomAccess = plainRandomAccess;
        File file = new File(filename);
        if (!file.canRead()) {
            if (filename.startsWith("file:/") || filename.startsWith("http://") || filename.startsWith("https://") || filename.startsWith("jar:")) {
                InputStream is = new URL(filename).openStream();
                try {
                    this.arrayIn = InputStreamToArray(is);
                    return;
                }
                finally {
                    try {is.close();}catch(IOException ioe){}
                }
            }
            else {
                InputStream is = BaseFont.getResourceStream(filename);
                if (is == null)
                    throw new IOException(filename + " not found as file or resource.");
                try {
                    this.arrayIn = InputStreamToArray(is);
                    return;
                }
                finally {
                    try {is.close();}catch(IOException ioe){}
                }
            }
        }
        else if (forceRead) {
            InputStream s = null;
            try {
                s = new FileInputStream(file);
                this.arrayIn = InputStreamToArray(s);
            }
            finally {
                try {if (s != null) {s.close();}}catch(Exception e){}
            }
            return;
        }
        this.filename = filename;
        if (plainRandomAccess)
            trf = new RandomAccessFile(filename, "r");
        else
            rf = new MappedRandomAccessFile(filename, "r");
    }

    public RandomAccessFileOrArray(URL url) throws IOException {
        InputStream is = url.openStream();
        try {
            this.arrayIn = InputStreamToArray(is);
        }
        finally {
            try {is.close();}catch(IOException ioe){}
        }
    }

    public RandomAccessFileOrArray(InputStream is) throws IOException {
        this.arrayIn = InputStreamToArray(is);
    }
    
    public static byte[] InputStreamToArray(InputStream is) throws IOException {
        byte b[] = new byte[8192];
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        while (true) {
            int read = is.read(b);
            if (read < 1)
                break;
            out.write(b, 0, read);
        }
        return out.toByteArray();
    }

    public RandomAccessFileOrArray(byte arrayIn[]) {
        this.arrayIn = arrayIn;
    }
    
    public RandomAccessFileOrArray(RandomAccessFileOrArray file) {
        filename = file.filename;
        arrayIn = file.arrayIn;
        startOffset = file.startOffset;
        plainRandomAccess = file.plainRandomAccess;
    }
    
    public void pushBack(byte b) {
        back = b;
        isBack = true;
    }
    
    public int read() throws IOException {
        if(isBack) {
            isBack = false;
            return back & 0xff;
        }
        if (arrayIn == null)
            return plainRandomAccess ? trf.read() : rf.read();
        else {
            if (arrayInPtr >= arrayIn.length)
                return -1;
            return arrayIn[arrayInPtr++] & 0xff;
        }
    }
    
    public int read(byte[] b, int off, int len) throws IOException {
        if (len == 0)
            return 0;
        int n = 0;
        if (isBack) {
            isBack = false;
            if (len == 1) {
                b[off] = back;
                return 1;
            }
            else {
                n = 1;
                b[off++] = back;
                --len;
            }
        }
        if (arrayIn == null) {
            return (plainRandomAccess ? trf.read(b, off, len) : rf.read(b, off, len)) + n;
        }
        else {
            if (arrayInPtr >= arrayIn.length)
                return -1;
            if (arrayInPtr + len > arrayIn.length)
                len = arrayIn.length - arrayInPtr;
            System.arraycopy(arrayIn, arrayInPtr, b, off, len);
            arrayInPtr += len;
            return len + n;
        }
    }
    
    public int read(byte b[]) throws IOException {
        return read(b, 0, b.length);
    }
    
    public void readFully(byte b[]) throws IOException {
        readFully(b, 0, b.length);
    }
    
    public void readFully(byte b[], int off, int len) throws IOException {
        int n = 0;
        do {
            int count = read(b, off + n, len - n);
            if (count < 0)
                throw new EOFException();
            n += count;
        } while (n < len);
    }
    
    public long skip(long n) throws IOException {
        return skipBytes((int)n);
    }
    
    public int skipBytes(int n) throws IOException {
        if (n <= 0) {
            return 0;
        }
        int adj = 0;
        if (isBack) {
            isBack = false;
            if (n == 1) {
                return 1;
            }
            else {
                --n;
                adj = 1;
            }
        }
        int pos;
        int len;
        int newpos;
        
        pos = getFilePointer();
        len = length();
        newpos = pos + n;
        if (newpos > len) {
            newpos = len;
        }
        seek(newpos);
        
        
        return newpos - pos + adj;
    }
    
    public void reOpen() throws IOException {
        if (filename != null && rf == null && trf == null) {
            if (plainRandomAccess)
                trf = new RandomAccessFile(filename, "r");
            else
                rf = new MappedRandomAccessFile(filename, "r");
        }
        seek(0);
    }
    
    protected void insureOpen() throws IOException {
        if (filename != null && rf == null && trf == null) {
            reOpen();
        }
    }
    
    public boolean isOpen() {
        return (filename == null || rf != null || trf != null);
    }
    
    public void close() throws IOException {
        isBack = false;
        if (rf != null) {
            rf.close();
            rf = null;
            
            
            
            plainRandomAccess = true;
        }
        else if (trf != null) {
            trf.close();
            trf = null;
        }
    }
    
    public int length() throws IOException {
        if (arrayIn == null) {
            insureOpen();
            return (int)(plainRandomAccess ? trf.length() : rf.length()) - startOffset;
        }
        else
            return arrayIn.length - startOffset;
    }
    
    public void seek(int pos) throws IOException {
        pos += startOffset;
        isBack = false;
        if (arrayIn == null) {
            insureOpen();
            if (plainRandomAccess)
                trf.seek(pos);
            else
                rf.seek(pos);
        }
        else
            arrayInPtr = pos;
    }
    
    public void seek(long pos) throws IOException {
        seek((int)pos);
    }
    
    public int getFilePointer() throws IOException {
        insureOpen();
        int n = isBack ? 1 : 0;
        if (arrayIn == null) {
            return (int)(plainRandomAccess ? trf.getFilePointer() : rf.getFilePointer()) - n - startOffset;
        }
        else
            return arrayInPtr - n - startOffset;
    }
    
    public boolean readBoolean() throws IOException {
        int ch = this.read();
        if (ch < 0)
            throw new EOFException();
        return (ch != 0);
    }
    
    public byte readByte() throws IOException {
        int ch = this.read();
        if (ch < 0)
            throw new EOFException();
        return (byte)(ch);
    }
    
    public int readUnsignedByte() throws IOException {
        int ch = this.read();
        if (ch < 0)
            throw new EOFException();
        return ch;
    }
    
    public short readShort() throws IOException {
        int ch1 = this.read();
        int ch2 = this.read();
        if ((ch1 | ch2) < 0)
            throw new EOFException();
        return (short)((ch1 << 8) + ch2);
    }
    
    
    public final short readShortLE() throws IOException {
        int ch1 = this.read();
        int ch2 = this.read();
        if ((ch1 | ch2) < 0)
            throw new EOFException();
        return (short)((ch2 << 8) + (ch1 << 0));
    }
    
    public int readUnsignedShort() throws IOException {
        int ch1 = this.read();
        int ch2 = this.read();
        if ((ch1 | ch2) < 0)
            throw new EOFException();
        return (ch1 << 8) + ch2;
    }
    
    
    public final int readUnsignedShortLE() throws IOException {
        int ch1 = this.read();
        int ch2 = this.read();
        if ((ch1 | ch2) < 0)
            throw new EOFException();
        return (ch2 << 8) + (ch1 << 0);
    }
    
    public char readChar() throws IOException {
        int ch1 = this.read();
        int ch2 = this.read();
        if ((ch1 | ch2) < 0)
            throw new EOFException();
        return (char)((ch1 << 8) + ch2);
    }
    
    
    public final char readCharLE() throws IOException {
        int ch1 = this.read();
        int ch2 = this.read();
        if ((ch1 | ch2) < 0)
            throw new EOFException();
        return (char)((ch2 << 8) + (ch1 << 0));
    }
    
    public int readInt() throws IOException {
        int ch1 = this.read();
        int ch2 = this.read();
        int ch3 = this.read();
        int ch4 = this.read();
        if ((ch1 | ch2 | ch3 | ch4) < 0)
            throw new EOFException();
        return ((ch1 << 24) + (ch2 << 16) + (ch3 << 8) + ch4);
    }
    
    
    public final int readIntLE() throws IOException {
        int ch1 = this.read();
        int ch2 = this.read();
        int ch3 = this.read();
        int ch4 = this.read();
        if ((ch1 | ch2 | ch3 | ch4) < 0)
            throw new EOFException();
        return ((ch4 << 24) + (ch3 << 16) + (ch2 << 8) + (ch1 << 0));
    }
    
    
    public final long readUnsignedInt() throws IOException {
        long ch1 = this.read();
        long ch2 = this.read();
        long ch3 = this.read();
        long ch4 = this.read();
        if ((ch1 | ch2 | ch3 | ch4) < 0)
            throw new EOFException();
        return ((ch1 << 24) + (ch2 << 16) + (ch3 << 8) + (ch4 << 0));
    }
    
    public final long readUnsignedIntLE() throws IOException {
        long ch1 = this.read();
        long ch2 = this.read();
        long ch3 = this.read();
        long ch4 = this.read();
        if ((ch1 | ch2 | ch3 | ch4) < 0)
            throw new EOFException();
        return ((ch4 << 24) + (ch3 << 16) + (ch2 << 8) + (ch1 << 0));
    }
    
    public long readLong() throws IOException {
        return ((long)(readInt()) << 32) + (readInt() & 0xFFFFFFFFL);
    }
    
    public final long readLongLE() throws IOException {
        int i1 = readIntLE();
        int i2 = readIntLE();
        return ((long)i2 << 32) + (i1 & 0xFFFFFFFFL);
    }
    
    public float readFloat() throws IOException {
        return Float.intBitsToFloat(readInt());
    }
    
    public final float readFloatLE() throws IOException {
        return Float.intBitsToFloat(readIntLE());
    }
    
    public double readDouble() throws IOException {
        return Double.longBitsToDouble(readLong());
    }
    
    public final double readDoubleLE() throws IOException {
        return Double.longBitsToDouble(readLongLE());
    }
    
    public String readLine() throws IOException {
        StringBuffer input = new StringBuffer();
        int c = -1;
        boolean eol = false;
        
        while (!eol) {
            switch (c = read()) {
                case -1:
                case '\n':
                    eol = true;
                    break;
                case '\r':
                    eol = true;
                    int cur = getFilePointer();
                    if ((read()) != '\n') {
                        seek(cur);
                    }
                    break;
                default:
                    input.append((char)c);
                    break;
            }
        }
        
        if ((c == -1) && (input.length() == 0)) {
            return null;
        }
        return input.toString();
    }
    
    public String readUTF() throws IOException {
        return DataInputStream.readUTF(this);
    }
    
    
    public int getStartOffset() {
        return this.startOffset;
    }
    
    
    public void setStartOffset(int startOffset) {
        this.startOffset = startOffset;
    }
    
}
