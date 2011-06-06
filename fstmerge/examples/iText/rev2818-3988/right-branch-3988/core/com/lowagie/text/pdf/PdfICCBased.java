
package com.lowagie.text.pdf;

import java.awt.color.ICC_Profile;

import com.lowagie.text.ExceptionConverter;



public class PdfICCBased extends PdfStream {

    
    public PdfICCBased(ICC_Profile profile) {
        this(profile, DEFAULT_COMPRESSION);
    }
    
    
    public PdfICCBased(ICC_Profile profile, int compressionLevel) {
        super();
        try {
            int numberOfComponents = profile.getNumComponents();
            switch (numberOfComponents) {
                case 1:
                    put(PdfName.ALTERNATE, PdfName.DEVICEGRAY);
                    break;
                case 3:
                    put(PdfName.ALTERNATE, PdfName.DEVICERGB);
                    break;
                case 4:
                    put(PdfName.ALTERNATE, PdfName.DEVICECMYK);
                    break;
                default:
                    throw new PdfException(numberOfComponents + " component(s) is not supported in PDF1.4");
            }
            put(PdfName.N, new PdfNumber(numberOfComponents));
            bytes = profile.getData();
            flateCompress(compressionLevel);
        } catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
}
