

package com.lowagie.text.pdf;

import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;



class PdfFont implements Comparable {
    
    
    
    private BaseFont font;
    
    
    private float size;
    
    
    protected Image image;
    
    protected float hScale = 1;
    
    
    
    PdfFont(BaseFont bf, float size) {
        this.size = size;
        font = bf;
    }
    
    
    
    
    
    public int compareTo(Object object) {
        if (image != null)
            return 0;
        if (object == null) {
            return -1;
        }
        PdfFont pdfFont;
        try {
            pdfFont = (PdfFont) object;
            if (font != pdfFont.font) {
                return 1;
            }
            if (this.size() != pdfFont.size()) {
                return 2;
            }
            return 0;
        }
        catch(ClassCastException cce) {
            return -2;
        }
    }
    
    
    
    float size() {
        if (image == null)
            return size;
        else {
            return image.getScaledHeight();
        }
    }
    
    
    
    float width() {
        return width(' ');
    }
    
    
    
    float width(int character) {
        if (image == null)
            return font.getWidthPoint(character, size) * hScale;
        else
            return image.getScaledWidth();
    }
    
    float width(String s) {
        if (image == null)
            return font.getWidthPoint(s, size) * hScale;
        else
            return image.getScaledWidth();
    }
    
    BaseFont getFont() {
        return font;
    }
    
    void setImage(Image image) {
        this.image = image;
    }
    
    static PdfFont getDefaultFont() {
        try {
            BaseFont bf = BaseFont.createFont(BaseFont.HELVETICA, BaseFont.WINANSI, false);
            return new PdfFont(bf, 12);
        }
        catch (Exception ee) {
            throw new ExceptionConverter(ee);
        }
    }
    void setHorizontalScaling(float hScale) {
        this.hScale = hScale;
    }
}
