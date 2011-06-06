

package com.lowagie.text;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;



public class Jpeg2000 extends Image {
    
    
    
    public static final int JP2_JP = 0x6a502020;
    public static final int JP2_IHDR = 0x69686472;
    public static final int JPIP_JPIP = 0x6a706970;

    public static final int JP2_FTYP = 0x66747970;
    public static final int JP2_JP2H = 0x6a703268;
    public static final int JP2_COLR = 0x636f6c72;
    public static final int JP2_JP2C = 0x6a703263;
    public static final int JP2_URL = 0x75726c20;
    public static final int JP2_DBTL = 0x6474626c;
    public static final int JP2_BPCC = 0x62706363;
    public static final int JP2_JP2 = 0x6a703220;

    InputStream inp;
    int boxLength;
    int boxType;
    
    
    
    Jpeg2000(Image image) {
        super(image);
    }

    
    public Jpeg2000(URL url) throws BadElementException, IOException {
        super(url);
        processParameters();
    }
    
    
    
    public Jpeg2000(byte[] img) throws BadElementException, IOException {
        super((URL)null);
        rawData = img;
        originalData = img;
        processParameters();
    }
    
    
    
    public Jpeg2000(byte[] img, float width, float height) throws BadElementException, IOException {
        this(img);
        scaledWidth = width;
        scaledHeight = height;
    }
    
    private int cio_read(int n) throws IOException {
        int v = 0;
        for (int i = n - 1; i >= 0; i--) {
            v += inp.read() << (i << 3);
        }
        return v;
    }
    
    public void jp2_read_boxhdr() throws IOException {
        boxLength = cio_read(4);
        boxType = cio_read(4);
        if (boxLength == 1) {
            if (cio_read(4) != 0) {
                throw new IOException("Cannot handle box sizes higher than 2^32");
            }
            boxLength = cio_read(4);
            if (boxLength == 0) 
                throw new IOException("Unsupported box size == 0");
        }
        else if (boxLength == 0) {
            throw new IOException("Unsupported box size == 0");
        }
    }
    
    
    private void processParameters() throws IOException {
        type = JPEG2000;
        originalType = ORIGINAL_JPEG2000;
        inp = null;
        try {
            String errorID;
            if (rawData == null){
                inp = url.openStream();
                errorID = url.toString();
            }
            else{
                inp = new java.io.ByteArrayInputStream(rawData);
                errorID = "Byte array";
            }
            boxLength = cio_read(4);
            if (boxLength == 0x0000000c) {
                boxType = cio_read(4);
                if (JP2_JP != boxType) {
                    throw new IOException("Expected JP Marker");
                }
                if (0x0d0a870a != cio_read(4)) {
                    throw new IOException("Error with JP Marker");
                }

                jp2_read_boxhdr();
                if (JP2_FTYP != boxType) {
                    throw new IOException("Expected FTYP Marker");
                }
                Utilities.skip(inp, boxLength - 8);
                jp2_read_boxhdr();
                do {
                    if (JP2_JP2H != boxType) {
                        if (boxType == JP2_JP2C) {
                            throw new IOException("Expected JP2H Marker");
                        }
                        Utilities.skip(inp, boxLength - 8);
                        jp2_read_boxhdr();
                    }
                } while(JP2_JP2H != boxType);
                jp2_read_boxhdr();
                if (JP2_IHDR != boxType) {
                    throw new IOException("Expected IHDR Marker");
                }
                scaledHeight = cio_read(4);
                setTop(scaledHeight);
                scaledWidth = cio_read(4);
                setRight(scaledWidth);
                bpc = -1;
            }
            else if (boxLength == 0xff4fff51) {
                Utilities.skip(inp, 4);
                int x1 = cio_read(4);
                int y1 = cio_read(4);
                int x0 = cio_read(4);
                int y0 = cio_read(4);
                Utilities.skip(inp, 16);
                colorspace = cio_read(2);
                bpc = 8;
                scaledHeight = y1 - y0;
                setTop(scaledHeight);
                scaledWidth = x1 - x0;
                setRight(scaledWidth);
            }
            else {
                throw new IOException("Not a valid Jpeg2000 file");
            }
        }
        finally {
            if (inp != null) {
                try{inp.close();}catch(Exception e){}
                inp = null;
            }
        }
        plainWidth = getWidth();
        plainHeight = getHeight();
    }
}
