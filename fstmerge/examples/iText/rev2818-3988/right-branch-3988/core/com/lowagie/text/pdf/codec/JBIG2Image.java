

package com.lowagie.text.pdf.codec;

import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.ImgJBIG2;
import com.lowagie.text.pdf.RandomAccessFileOrArray;


public class JBIG2Image {

    
    public static byte[] getGlobalSegment(RandomAccessFileOrArray ra ) {
        try {
            JBIG2SegmentReader sr = new JBIG2SegmentReader(ra);
            sr.read();
            return sr.getGlobal(true);
        } catch (Exception e) {
            return null;
        }
    }
    
    
    public static Image getJbig2Image(RandomAccessFileOrArray ra, int page) {
        if (page < 1)
            throw new IllegalArgumentException("The page number must be >= 1.");
        
        try {
            JBIG2SegmentReader sr = new JBIG2SegmentReader(ra);
            sr.read();
            JBIG2SegmentReader.JBIG2Page p = sr.getPage(page);
            Image img = new ImgJBIG2(p.pageBitmapWidth, p.pageBitmapHeight, p.getData(true), sr.getGlobal(true));
            return img;
        } catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    
    public static int getNumberOfPages(RandomAccessFileOrArray ra) {
        try {
            JBIG2SegmentReader sr = new JBIG2SegmentReader(ra);
            sr.read();
            return sr.numberOfPages();
        } catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
    
}
