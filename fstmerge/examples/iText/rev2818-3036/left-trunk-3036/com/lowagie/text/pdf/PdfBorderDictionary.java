

package com.lowagie.text.pdf;



public class PdfBorderDictionary extends PdfDictionary {
    
    public static final int STYLE_SOLID = 0;
    public static final int STYLE_DASHED = 1;
    public static final int STYLE_BEVELED = 2;
    public static final int STYLE_INSET = 3;
    public static final int STYLE_UNDERLINE = 4;
    
    

    
    public PdfBorderDictionary(float borderWidth, int borderStyle, PdfDashPattern dashes) {
        put(PdfName.W, new PdfNumber(borderWidth));
        switch (borderStyle) {
            case STYLE_SOLID:
                put(PdfName.S, PdfName.S);
                break;
            case STYLE_DASHED:
                if (dashes != null)
                    put(PdfName.D, dashes);
                put(PdfName.S, PdfName.D);
                break;
            case STYLE_BEVELED:
                put(PdfName.S, PdfName.B);
                break;
            case STYLE_INSET:
                put(PdfName.S, PdfName.I);
                break;
            case STYLE_UNDERLINE:
                put(PdfName.S, PdfName.U);
                break;
            default:
                throw new IllegalArgumentException("Invalid border style.");
        }
    }
    
    public PdfBorderDictionary(float borderWidth, int borderStyle) {
        this(borderWidth, borderStyle, null);
    }
}