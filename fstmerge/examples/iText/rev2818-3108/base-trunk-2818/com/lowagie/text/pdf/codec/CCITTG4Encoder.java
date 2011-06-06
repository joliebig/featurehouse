
package com.lowagie.text.pdf.codec;

import com.lowagie.text.pdf.ByteBuffer;


public class CCITTG4Encoder {
    private int rowbytes;
    private int rowpixels;
    private int bit = 8;
    private int data;
    private byte[] refline;
    private ByteBuffer outBuf = new ByteBuffer(1024);
    private byte[] dataBp;
    private int offsetData;
    private int sizeData;
    
        
    public CCITTG4Encoder(int width) {
        rowpixels = width;
        rowbytes = (rowpixels + 7) / 8;
        refline = new byte[rowbytes];
    }
    
        
    public void fax4Encode(byte[] data, int offset, int size) {
        dataBp = data;
        offsetData = offset;
        sizeData = size;
        while (sizeData > 0) {
            Fax3Encode2DRow();
            System.arraycopy(dataBp, offsetData, refline, 0, rowbytes);
            offsetData += rowbytes;
            sizeData -= rowbytes;
        }
    }
    

        
    public static byte[] compress(byte[] data, int width, int height) {
        CCITTG4Encoder g4 = new CCITTG4Encoder(width);
        g4.fax4Encode(data, 0, g4.rowbytes * height);
        return g4.close();
    }
    
        
    public void fax4Encode(byte[] data, int height) {
        fax4Encode(data, 0, rowbytes * height);
    }

    private void putcode(int[] table) {
        putBits(table[CODE], table[LENGTH]);
    }
    
    private void putspan(int span, int[][] tab) {
        int code, length;
        
        while (span >= 2624) {
            int[] te = tab[63 + (2560>>6)];
            code = te[CODE];
            length = te[LENGTH];
            putBits(code, length);
            span -= te[RUNLEN];
        }
        if (span >= 64) {
            int[] te = tab[63 + (span>>6)];
            code = te[CODE];
            length = te[LENGTH];
            putBits(code, length);
            span -= te[RUNLEN];
        }
        code = tab[span][CODE];
        length = tab[span][LENGTH];
        putBits(code, length);
    }
    
    private void putBits(int bits, int length) {
        while (length > bit) {
            data |= bits >> (length - bit);
            length -= bit;
            outBuf.append((byte)data);
            data = 0;
            bit = 8;
        }
        data |= (bits & msbmask[length]) << (bit - length);
        bit -= length;
        if (bit == 0) {
            outBuf.append((byte)data);
            data = 0;
            bit = 8;
        }
    }
    
    private void Fax3Encode2DRow() {
        int a0 = 0;
        int a1 = (pixel(dataBp, offsetData, 0) != 0 ? 0 : finddiff(dataBp, offsetData, 0, rowpixels, 0));
        int b1 = (pixel(refline, 0, 0) != 0 ? 0 : finddiff(refline, 0, 0, rowpixels, 0));
        int a2, b2;
        
        for (;;) {
            b2 = finddiff2(refline, 0, b1, rowpixels, pixel(refline, 0,b1));
            if (b2 >= a1) {
                int d = b1 - a1;
                if (!(-3 <= d && d <= 3)) {    
                    a2 = finddiff2(dataBp, offsetData, a1, rowpixels, pixel(dataBp, offsetData,a1));
                    putcode(horizcode);
                    if (a0+a1 == 0 || pixel(dataBp, offsetData, a0) == 0) {
                        putspan(a1-a0, TIFFFaxWhiteCodes);
                        putspan(a2-a1, TIFFFaxBlackCodes);
                    } else {
                        putspan(a1-a0, TIFFFaxBlackCodes);
                        putspan(a2-a1, TIFFFaxWhiteCodes);
                    }
                    a0 = a2;
                } else {            
                    putcode(vcodes[d+3]);
                    a0 = a1;
                }
            } else {                
                putcode(passcode);
                a0 = b2;
            }
            if (a0 >= rowpixels)
                break;
            a1 = finddiff(dataBp, offsetData, a0, rowpixels, pixel(dataBp, offsetData,a0));
            b1 = finddiff(refline, 0, a0, rowpixels, pixel(dataBp, offsetData,a0) ^ 1);
            b1 = finddiff(refline, 0, b1, rowpixels, pixel(dataBp, offsetData,a0));
        }
    }
    
    private void Fax4PostEncode() {
        putBits(EOL, 12);
        putBits(EOL, 12);
        if (bit != 8) {
            outBuf.append((byte)data);
            data = 0;
            bit = 8;
        }
    }
    
        
    public byte[] close() {
        Fax4PostEncode();
        return outBuf.toByteArray();
    }
    
    private int pixel(byte[] data, int offset, int bit) {
        if (bit >= rowpixels)
            return 0;
        return ((data[offset + (bit >> 3)] & 0xff) >> (7-((bit)&7))) & 1;
    }
    
    private static int find1span(byte[] bp, int offset, int bs, int be) {
        int bits = be - bs;
        int n, span;
        
        int pos = offset + (bs >> 3);
        
        if (bits > 0 && (n = (bs & 7)) != 0) {
            span = oneruns[((int)bp[pos] << n) & 0xff];
            if (span > 8-n)        
                span = 8-n;
            if (span > bits)    
                span = bits;
            if (n+span < 8)        
                return span;
            bits -= span;
            pos++;
        } else
            span = 0;
        
        while (bits >= 8) {
            if (bp[pos] != -1)    
                return (span + oneruns[bp[pos] & 0xff]);
            span += 8;
            bits -= 8;
            pos++;
        }
        
        if (bits > 0) {
            n = oneruns[bp[pos] & 0xff];
            span += (n > bits ? bits : n);
        }
        return span;
    }
    
    private static int find0span(byte[] bp, int offset, int bs, int be) {
        int bits = be - bs;
        int n, span;
        
        int pos = offset + (bs >> 3);
        
        if (bits > 0 && (n = (bs & 7)) != 0) {
            span = zeroruns[((int)bp[pos] << n) & 0xff];
            if (span > 8-n)        
                span = 8-n;
            if (span > bits)    
                span = bits;
            if (n+span < 8)        
                return span;
            bits -= span;
            pos++;
        } else
            span = 0;
        
        while (bits >= 8) {
            if (bp[pos] != 0)    
                return (span + zeroruns[bp[pos] & 0xff]);
            span += 8;
            bits -= 8;
            pos++;
        }
        
        if (bits > 0) {
            n = zeroruns[bp[pos] & 0xff];
            span += (n > bits ? bits : n);
        }
        return span;
    }
    
    private static int finddiff(byte[] bp, int offset, int bs, int be, int color) {
        return bs + (color != 0 ? find1span(bp, offset, bs, be) : find0span(bp, offset, bs, be));
    }
    
    private static int finddiff2(byte[] bp, int offset, int bs, int be, int color) {
        return bs < be ? finddiff(bp, offset, bs, be, color) : be;
    }
    
    private static byte zeroruns[] = {
        8, 7, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4,    
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,    
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,    
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,    
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0    
    };
    
    private static byte oneruns[] = {
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,    
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,    
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,    
        4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 7, 8    
    };

    private static final int LENGTH = 0; 
    private static final int CODE = 1;   
    private static final int RUNLEN = 2; 

    private static final int EOL = 0x001; 

    
    private static final int G3CODE_EOL    = -1;     
    private static final int G3CODE_INVALID = -2; 
    private static final int G3CODE_EOF = -3;     
    private static final int G3CODE_INCOMP = -4;  

    private int[][] TIFFFaxWhiteCodes = {
        { 8, 0x35, 0 },    
        { 6, 0x7, 1 },    
        { 4, 0x7, 2 },    
        { 4, 0x8, 3 },    
        { 4, 0xB, 4 },    
        { 4, 0xC, 5 },    
        { 4, 0xE, 6 },    
        { 4, 0xF, 7 },    
        { 5, 0x13, 8 },    
        { 5, 0x14, 9 },    
        { 5, 0x7, 10 },    
        { 5, 0x8, 11 },    
        { 6, 0x8, 12 },    
        { 6, 0x3, 13 },    
        { 6, 0x34, 14 },    
        { 6, 0x35, 15 },    
        { 6, 0x2A, 16 },    
        { 6, 0x2B, 17 },    
        { 7, 0x27, 18 },    
        { 7, 0xC, 19 },    
        { 7, 0x8, 20 },    
        { 7, 0x17, 21 },    
        { 7, 0x3, 22 },    
        { 7, 0x4, 23 },    
        { 7, 0x28, 24 },    
        { 7, 0x2B, 25 },    
        { 7, 0x13, 26 },    
        { 7, 0x24, 27 },    
        { 7, 0x18, 28 },    
        { 8, 0x2, 29 },    
        { 8, 0x3, 30 },    
        { 8, 0x1A, 31 },    
        { 8, 0x1B, 32 },    
        { 8, 0x12, 33 },    
        { 8, 0x13, 34 },    
        { 8, 0x14, 35 },    
        { 8, 0x15, 36 },    
        { 8, 0x16, 37 },    
        { 8, 0x17, 38 },    
        { 8, 0x28, 39 },    
        { 8, 0x29, 40 },    
        { 8, 0x2A, 41 },    
        { 8, 0x2B, 42 },    
        { 8, 0x2C, 43 },    
        { 8, 0x2D, 44 },    
        { 8, 0x4, 45 },    
        { 8, 0x5, 46 },    
        { 8, 0xA, 47 },    
        { 8, 0xB, 48 },    
        { 8, 0x52, 49 },    
        { 8, 0x53, 50 },    
        { 8, 0x54, 51 },    
        { 8, 0x55, 52 },    
        { 8, 0x24, 53 },    
        { 8, 0x25, 54 },    
        { 8, 0x58, 55 },    
        { 8, 0x59, 56 },    
        { 8, 0x5A, 57 },    
        { 8, 0x5B, 58 },    
        { 8, 0x4A, 59 },    
        { 8, 0x4B, 60 },    
        { 8, 0x32, 61 },    
        { 8, 0x33, 62 },    
        { 8, 0x34, 63 },    
        { 5, 0x1B, 64 },    
        { 5, 0x12, 128 },    
        { 6, 0x17, 192 },    
        { 7, 0x37, 256 },    
        { 8, 0x36, 320 },    
        { 8, 0x37, 384 },    
        { 8, 0x64, 448 },    
        { 8, 0x65, 512 },    
        { 8, 0x68, 576 },    
        { 8, 0x67, 640 },    
        { 9, 0xCC, 704 },    
        { 9, 0xCD, 768 },    
        { 9, 0xD2, 832 },    
        { 9, 0xD3, 896 },    
        { 9, 0xD4, 960 },    
        { 9, 0xD5, 1024 },    
        { 9, 0xD6, 1088 },    
        { 9, 0xD7, 1152 },    
        { 9, 0xD8, 1216 },    
        { 9, 0xD9, 1280 },    
        { 9, 0xDA, 1344 },    
        { 9, 0xDB, 1408 },    
        { 9, 0x98, 1472 },    
        { 9, 0x99, 1536 },    
        { 9, 0x9A, 1600 },    
        { 6, 0x18, 1664 },    
        { 9, 0x9B, 1728 },    
        { 11, 0x8, 1792 },    
        { 11, 0xC, 1856 },    
        { 11, 0xD, 1920 },    
        { 12, 0x12, 1984 },    
        { 12, 0x13, 2048 },    
        { 12, 0x14, 2112 },    
        { 12, 0x15, 2176 },    
        { 12, 0x16, 2240 },    
        { 12, 0x17, 2304 },    
        { 12, 0x1C, 2368 },    
        { 12, 0x1D, 2432 },    
        { 12, 0x1E, 2496 },    
        { 12, 0x1F, 2560 },    
        { 12, 0x1, G3CODE_EOL },    
        { 9, 0x1, G3CODE_INVALID },    
        { 10, 0x1, G3CODE_INVALID },    
        { 11, 0x1, G3CODE_INVALID },    
        { 12, 0x0, G3CODE_INVALID }    
    };

    private int[][] TIFFFaxBlackCodes = {
        { 10, 0x37, 0 },    
        { 3, 0x2, 1 },    
        { 2, 0x3, 2 },    
        { 2, 0x2, 3 },    
        { 3, 0x3, 4 },    
        { 4, 0x3, 5 },    
        { 4, 0x2, 6 },    
        { 5, 0x3, 7 },    
        { 6, 0x5, 8 },    
        { 6, 0x4, 9 },    
        { 7, 0x4, 10 },    
        { 7, 0x5, 11 },    
        { 7, 0x7, 12 },    
        { 8, 0x4, 13 },    
        { 8, 0x7, 14 },    
        { 9, 0x18, 15 },    
        { 10, 0x17, 16 },    
        { 10, 0x18, 17 },    
        { 10, 0x8, 18 },    
        { 11, 0x67, 19 },    
        { 11, 0x68, 20 },    
        { 11, 0x6C, 21 },    
        { 11, 0x37, 22 },    
        { 11, 0x28, 23 },    
        { 11, 0x17, 24 },    
        { 11, 0x18, 25 },    
        { 12, 0xCA, 26 },    
        { 12, 0xCB, 27 },    
        { 12, 0xCC, 28 },    
        { 12, 0xCD, 29 },    
        { 12, 0x68, 30 },    
        { 12, 0x69, 31 },    
        { 12, 0x6A, 32 },    
        { 12, 0x6B, 33 },    
        { 12, 0xD2, 34 },    
        { 12, 0xD3, 35 },    
        { 12, 0xD4, 36 },    
        { 12, 0xD5, 37 },    
        { 12, 0xD6, 38 },    
        { 12, 0xD7, 39 },    
        { 12, 0x6C, 40 },    
        { 12, 0x6D, 41 },    
        { 12, 0xDA, 42 },    
        { 12, 0xDB, 43 },    
        { 12, 0x54, 44 },    
        { 12, 0x55, 45 },    
        { 12, 0x56, 46 },    
        { 12, 0x57, 47 },    
        { 12, 0x64, 48 },    
        { 12, 0x65, 49 },    
        { 12, 0x52, 50 },    
        { 12, 0x53, 51 },    
        { 12, 0x24, 52 },    
        { 12, 0x37, 53 },    
        { 12, 0x38, 54 },    
        { 12, 0x27, 55 },    
        { 12, 0x28, 56 },    
        { 12, 0x58, 57 },    
        { 12, 0x59, 58 },    
        { 12, 0x2B, 59 },    
        { 12, 0x2C, 60 },    
        { 12, 0x5A, 61 },    
        { 12, 0x66, 62 },    
        { 12, 0x67, 63 },    
        { 10, 0xF, 64 },    
        { 12, 0xC8, 128 },    
        { 12, 0xC9, 192 },    
        { 12, 0x5B, 256 },    
        { 12, 0x33, 320 },    
        { 12, 0x34, 384 },    
        { 12, 0x35, 448 },    
        { 13, 0x6C, 512 },    
        { 13, 0x6D, 576 },    
        { 13, 0x4A, 640 },    
        { 13, 0x4B, 704 },    
        { 13, 0x4C, 768 },    
        { 13, 0x4D, 832 },    
        { 13, 0x72, 896 },    
        { 13, 0x73, 960 },    
        { 13, 0x74, 1024 },    
        { 13, 0x75, 1088 },    
        { 13, 0x76, 1152 },    
        { 13, 0x77, 1216 },    
        { 13, 0x52, 1280 },    
        { 13, 0x53, 1344 },    
        { 13, 0x54, 1408 },    
        { 13, 0x55, 1472 },    
        { 13, 0x5A, 1536 },    
        { 13, 0x5B, 1600 },    
        { 13, 0x64, 1664 },    
        { 13, 0x65, 1728 },    
        { 11, 0x8, 1792 },    
        { 11, 0xC, 1856 },    
        { 11, 0xD, 1920 },    
        { 12, 0x12, 1984 },    
        { 12, 0x13, 2048 },    
        { 12, 0x14, 2112 },    
        { 12, 0x15, 2176 },    
        { 12, 0x16, 2240 },    
        { 12, 0x17, 2304 },    
        { 12, 0x1C, 2368 },    
        { 12, 0x1D, 2432 },    
        { 12, 0x1E, 2496 },    
        { 12, 0x1F, 2560 },    
        { 12, 0x1, G3CODE_EOL },    
        { 9, 0x1, G3CODE_INVALID },    
        { 10, 0x1, G3CODE_INVALID },    
        { 11, 0x1, G3CODE_INVALID },    
        { 12, 0x0, G3CODE_INVALID }    
    };
    
    private int[] horizcode =
        { 3, 0x1, 0 };        
    private int[] passcode =
        { 4, 0x1, 0 };        
    private int[][] vcodes = {
        { 7, 0x03, 0 },    
        { 6, 0x03, 0 },    
        { 3, 0x03, 0 },    
        { 1, 0x1, 0 },        
        { 3, 0x2, 0 },        
        { 6, 0x02, 0 },    
        { 7, 0x02, 0 }        
    };
    private int[] msbmask =
    { 0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff };
}
