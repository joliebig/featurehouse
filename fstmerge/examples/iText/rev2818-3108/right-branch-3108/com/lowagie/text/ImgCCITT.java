

package com.lowagie.text;

import com.lowagie.text.pdf.codec.TIFFFaxDecoder;
import java.net.URL;



public class ImgCCITT extends Image {

    ImgCCITT(Image image) {
        super(image);
    }

    

    public ImgCCITT(int width, int height, boolean reverseBits, int typeCCITT, int parameters, byte[] data) throws BadElementException{
        super((URL)null);
        if (typeCCITT != CCITTG4 && typeCCITT != CCITTG3_1D && typeCCITT != CCITTG3_2D)
            throw new BadElementException("The CCITT compression type must be CCITTG4, CCITTG3_1D or CCITTG3_2D");
        if (reverseBits)
            TIFFFaxDecoder.reverseBits(data);
        type = IMGRAW;
        scaledHeight = height;
        setTop(scaledHeight);
        scaledWidth = width;
        setRight(scaledWidth);
        colorspace = parameters;
        bpc = typeCCITT;
        rawData = data;
        plainWidth = getWidth();
        plainHeight = getHeight();
    }
}
