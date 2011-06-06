
package com.lowagie.text.pdf.codec;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;

import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.ImgRaw;
import com.lowagie.text.Utilities;
import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;
import com.lowagie.text.pdf.PdfString;


public class GifImage {
    
    protected DataInputStream in;
    protected int width;            
    protected int height;           
    protected boolean gctFlag;      

    protected int bgIndex;          
    protected int bgColor;          
    protected int pixelAspect;      

    protected boolean lctFlag;      
    protected boolean interlace;    
    protected int lctSize;          

    protected int ix, iy, iw, ih;   

    protected byte[] block = new byte[256];  
    protected int blockSize = 0;    

    
    protected int dispose = 0;   
    protected boolean transparency = false;   
    protected int delay = 0;        
    protected int transIndex;       

    protected static final int MaxStackSize = 4096;   

    
    protected short[] prefix;
    protected byte[] suffix;
    protected byte[] pixelStack;
    protected byte[] pixels;

    protected byte m_out[];
    protected int m_bpc;
    protected int m_gbpc;
    protected byte m_global_table[];
    protected byte m_local_table[];
    protected byte m_curr_table[];
    protected int m_line_stride;
    protected byte fromData[];
    protected URL fromUrl;


    protected ArrayList<GifFrame> frames = new ArrayList<GifFrame>();     

        
    public GifImage(URL url) throws IOException {
        fromUrl = url;
        InputStream is = null;
        try {
            is = url.openStream();
            process(is);
        }
        finally {
            if (is != null) {
                is.close();
            }
        }
    }
    
        
    public GifImage(String file) throws IOException {
        this(Utilities.toURL(file));
    }
    
        
    public GifImage(byte data[]) throws IOException {
        fromData = data;
        InputStream is = null;
        try {
            is = new ByteArrayInputStream(data);
            process(is);
        }
        finally {
            if (is != null) {
                is.close();
            }
        }
    }
    
        
    public GifImage(InputStream is) throws IOException {
        process(is);
    }
    
        
    public int getFrameCount() {
        return frames.size();
    }
    
        
    public Image getImage(int frame) {
        GifFrame gf = frames.get(frame - 1);
        return gf.image;
    }
    
        
    public int[] getFramePosition(int frame) {
        GifFrame gf = frames.get(frame - 1);
        return new int[]{gf.ix, gf.iy};
        
    }
    
        
    public int[] getLogicalScreen() {
        return new int[]{width, height};
    }
    
    void process(InputStream is) throws IOException {
        in = new DataInputStream(new BufferedInputStream(is));
        readHeader();
        readContents();
        if (frames.isEmpty())
            throw new IOException("The file does not contain any valid image.");
    }
    
    
    protected void readHeader() throws IOException {
        String id = "";
        for (int i = 0; i < 6; i++)
            id += (char)in.read();
        if (!id.startsWith("GIF8")) {
            throw new IOException("Gif signature nor found.");
        }
        
        readLSD();
        if (gctFlag) {
            m_global_table = readColorTable(m_gbpc);
        }
    }

    
    protected void readLSD() throws IOException {
        
        
        width = readShort();
        height = readShort();
        
        
        int packed = in.read();
        gctFlag = (packed & 0x80) != 0;      
        m_gbpc = (packed & 7) + 1;
        bgIndex = in.read();        
        pixelAspect = in.read();    
    }

    
    protected int readShort() throws IOException {
        
        return in.read() | (in.read() << 8);
    }

    
    protected int readBlock() throws IOException {
        blockSize = in.read();
        if (blockSize <= 0)
            return blockSize = 0;
        for (int k = 0; k < blockSize; ++k) {
            int v = in.read();
            if (v < 0) {
                return blockSize = k;
            }
            block[k] = (byte)v;
        }
        return blockSize;
    }

    protected byte[] readColorTable(int bpc) throws IOException {
        int ncolors = 1 << bpc;
        int nbytes = 3*ncolors;
        bpc = newBpc(bpc);
        byte table[] = new byte[(1 << bpc) * 3];
        in.readFully(table, 0, nbytes);
        return table;
    }
 
    
    static protected int newBpc(int bpc) {
        switch (bpc) {
            case 1:
            case 2:
            case 4:
                break;
            case 3:
                return 4;
            default:
                return 8;
        }
        return bpc;
    }
    
    protected void readContents() throws IOException {
        
        boolean done = false;
        while (!done) {
            int code = in.read();
            switch (code) {
                
                case 0x2C:    
                    readImage();
                    break;
                    
                case 0x21:    
                    code = in.read();
                    switch (code) {
                        
                        case 0xf9:    
                            readGraphicControlExt();
                            break;
                            
                        case 0xff:    
                            readBlock();
                            skip();        
                            break;
                            
                        default:    
                            skip();
                    }
                    break;
                    
                default:
                    done = true;
                    break;
            }
        }
    }

    
    protected void readImage() throws IOException {
        ix = readShort();    
        iy = readShort();
        iw = readShort();
        ih = readShort();
        
        int packed = in.read();
        lctFlag = (packed & 0x80) != 0;     
        interlace = (packed & 0x40) != 0;   
        
        
        lctSize = 2 << (packed & 7);        
        m_bpc = newBpc(m_gbpc);
        if (lctFlag) {
            m_curr_table = readColorTable((packed & 7) + 1);   
            m_bpc = newBpc((packed & 7) + 1);
        }
        else {
            m_curr_table = m_global_table;
        }
        if (transparency && transIndex >= m_curr_table.length / 3)
            transparency = false;
        if (transparency && m_bpc == 1) { 
            byte tp[] = new byte[12];
            System.arraycopy(m_curr_table, 0, tp, 0, 6);
            m_curr_table = tp;
            m_bpc = 2;
        }
        boolean skipZero = decodeImageData();   
        if (!skipZero)
            skip();
        
        Image img = null;
        try {
            img = new ImgRaw(iw, ih, 1, m_bpc, m_out);
            PdfArray colorspace = new PdfArray();
            colorspace.add(PdfName.INDEXED);
            colorspace.add(PdfName.DEVICERGB);
            int len = m_curr_table.length;
            colorspace.add(new PdfNumber(len / 3 - 1));
            colorspace.add(new PdfString(m_curr_table));
            PdfDictionary ad = new PdfDictionary();
            ad.put(PdfName.COLORSPACE, colorspace);
            img.setAdditional(ad);
            if (transparency) {
                img.setTransparency(new int[]{transIndex, transIndex});
            }
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
        img.setOriginalType(Image.ORIGINAL_GIF);
        img.setOriginalData(fromData);
        img.setUrl(fromUrl);
        GifFrame gf = new GifFrame();
        gf.image = img;
        gf.ix = ix;
        gf.iy = iy;
        frames.add(gf);   
        
        
        
    }
    
    protected boolean decodeImageData() throws IOException {
        int NullCode = -1;
        int npix = iw * ih;
        int available, clear, code_mask, code_size, end_of_information, in_code, old_code,
        bits, code, count, i, datum, data_size, first, top, bi;
        boolean skipZero = false;
        
        if (prefix == null)
            prefix = new short[MaxStackSize];
        if (suffix == null)
            suffix = new byte[MaxStackSize];
        if (pixelStack == null)
            pixelStack = new byte[MaxStackSize+1];
        
        m_line_stride = (iw * m_bpc + 7) / 8;
        m_out = new byte[m_line_stride * ih];
        int pass = 1;
        int inc = interlace ? 8 : 1;
        int line = 0;
        int xpos = 0;
        
        
        
        data_size = in.read();
        clear = 1 << data_size;
        end_of_information = clear + 1;
        available = clear + 2;
        old_code = NullCode;
        code_size = data_size + 1;
        code_mask = (1 << code_size) - 1;
        for (code = 0; code < clear; code++) {
            prefix[code] = 0;
            suffix[code] = (byte) code;
        }
        
        
        
        datum = bits = count = first = top = bi = 0;
        
        for (i = 0; i < npix; ) {
            if (top == 0) {
                if (bits < code_size) {
                    
                    if (count == 0) {
                        
                        count = readBlock();
                        if (count <= 0) {
                            skipZero = true;
                            break;
                        }
                        bi = 0;
                    }
                    datum += (block[bi] & 0xff) << bits;
                    bits += 8;
                    bi++;
                    count--;
                    continue;
                }
                
                
                
                code = datum & code_mask;
                datum >>= code_size;
                bits -= code_size;
                
                
                
                if ((code > available) || (code == end_of_information))
                    break;
                if (code == clear) {
                    
                    code_size = data_size + 1;
                    code_mask = (1 << code_size) - 1;
                    available = clear + 2;
                    old_code = NullCode;
                    continue;
                }
                if (old_code == NullCode) {
                    pixelStack[top++] = suffix[code];
                    old_code = code;
                    first = code;
                    continue;
                }
                in_code = code;
                if (code == available) {
                    pixelStack[top++] = (byte) first;
                    code = old_code;
                }
                while (code > clear) {
                    pixelStack[top++] = suffix[code];
                    code = prefix[code];
                }
                first = suffix[code] & 0xff;
                
                
                
                if (available >= MaxStackSize)
                    break;
                pixelStack[top++] = (byte) first;
                prefix[available] = (short) old_code;
                suffix[available] = (byte) first;
                available++;
                if (((available & code_mask) == 0) && (available < MaxStackSize)) {
                    code_size++;
                    code_mask += available;
                }
                old_code = in_code;
            }
            
            
            
            top--;
            i++;
            
            setPixel(xpos, line, pixelStack[top]);
            ++xpos;
            if (xpos >= iw) {
                xpos = 0;
                line += inc;
                if (line >= ih) {
                    if (interlace) {
                        do {
                            pass++;
                            switch (pass) {
                                case 2:
                                    line = 4;
                                    break;
                                case 3:
                                    line = 2;
                                    inc = 4;
                                    break;
                                case 4:
                                    line = 1;
                                    inc = 2;
                                    break;
                                default: 
                                    line = ih - 1;
                                    inc = 0;
                            }
                        } while (line >= ih);
                    }
                    else {
                        line = ih - 1; 
                        inc = 0;
                    }
                }
            }
        }
        return skipZero;
    }
    
    
    protected void setPixel(int x, int y, int v) {
        if (m_bpc == 8) {
            int pos = x + iw * y;
            m_out[pos] = (byte)v;
        }
        else {
            int pos = m_line_stride * y + x / (8 / m_bpc);
            int vout = v << (8 - m_bpc * (x % (8 / m_bpc))- m_bpc);
            m_out[pos] |= vout;
        }
    }
    
    
    protected void resetFrame() {
        
        
        
    }

    
    protected void readGraphicControlExt() throws IOException {
        in.read();    
        int packed = in.read();   
        dispose = (packed & 0x1c) >> 2;   
        if (dispose == 0)
            dispose = 1;   
        transparency = (packed & 1) != 0;
        delay = readShort() * 10;   
        transIndex = in.read();        
        in.read();                     
    }
    
    
    protected void skip() throws IOException {
        do {
            readBlock();
        } while (blockSize > 0);
    }

    static class GifFrame {
        Image image;
        int ix;
        int iy;
    }
}
