

package com.lowagie.text;

import java.net.URL;
import java.security.MessageDigest;


public class ImgJBIG2 extends Image {
    
    
    private  byte[] global;
    
    private  byte[] globalHash;
    
    
    ImgJBIG2(Image image) {
        super(image);
    }

    
    public ImgJBIG2() {
        super((Image) null);
    }

    
    public ImgJBIG2(int width, int height, byte[] data, byte[] globals) {
        super((URL) null);
        type = JBIG2;
        originalType = ORIGINAL_JBIG2;
        scaledHeight = height;
        setTop(scaledHeight);
        scaledWidth = width;
        setRight(scaledWidth);
        bpc = 1;
        colorspace = 1;
        rawData = data;
        plainWidth = getWidth();
        plainHeight = getHeight();
        if ( globals != null ) {
            this.global = globals;
            MessageDigest md;
            try {
                md = MessageDigest.getInstance("MD5");
                md.update(this.global);
                this.globalHash = md.digest();
            } catch (Exception e) {
                
            }
            
        }
    }
    
    
    public byte[] getGlobalBytes() {
        return this.global;
    }
    
    
    public byte[] getGlobalHash() {
        return this.globalHash;
    }

}
