

package com.lowagie.text.pdf;

import java.awt.Color;



public class PdfSpotColor{
    

    protected float tint;
    

    public PdfName name;
    

    public Color altcs;
    
    
    
    
    public PdfSpotColor(String name, float tint, Color altcs) {
        this.name = new PdfName(name);
        this.tint = tint;
        this.altcs = altcs;
    }
    
    
    public float getTint() {
        return tint;
    }
    
    
    public Color getAlternativeCS() {
        return altcs;
    }
    
    protected PdfObject getSpotObject(PdfWriter writer) {
        PdfArray array = new PdfArray(PdfName.SEPARATION);
        array.add(name);
        PdfFunction func = null;
        if (altcs instanceof ExtendedColor) {
            int type = ((ExtendedColor)altcs).type;
            switch (type) {
                case ExtendedColor.TYPE_GRAY:
                    array.add(PdfName.DEVICEGRAY);
                    func = PdfFunction.type2(writer, new float[]{0, 1}, null, new float[]{0}, new float[]{((GrayColor)altcs).getGray()}, 1);
                    break;
                case ExtendedColor.TYPE_CMYK:
                    array.add(PdfName.DEVICECMYK);
                    CMYKColor cmyk = (CMYKColor)altcs;
                    func = PdfFunction.type2(writer, new float[]{0, 1}, null, new float[]{0, 0, 0, 0},
                        new float[]{cmyk.getCyan(), cmyk.getMagenta(), cmyk.getYellow(), cmyk.getBlack()}, 1);
                    break;
                default:
                    throw new RuntimeException("Only RGB, Gray and CMYK are supported as alternative color spaces.");
            }
        }
        else {
            array.add(PdfName.DEVICERGB);
            func = PdfFunction.type2(writer, new float[]{0, 1}, null, new float[]{1, 1, 1},
                new float[]{(float)altcs.getRed() / 255, (float)altcs.getGreen() / 255, (float)altcs.getBlue() / 255}, 1);
        }
        array.add(func.getReference());
        return array;
    }
}
