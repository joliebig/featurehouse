

package com.lowagie.text.pdf;

import com.lowagie.text.Rectangle;



public class PdfRectangle extends PdfArray {
    
    
    

    private float llx = 0;
    

    private float lly = 0;
    

    private float urx = 0;
    

    private float ury = 0;
    
    
    

    
    public PdfRectangle(float llx, float lly, float urx, float ury, int rotation) {
        super();
        if (rotation == 90 || rotation == 270) {
            this.llx = lly;
            this.lly = llx;
            this.urx = ury;
            this.ury = urx;
        }
        else {
            this.llx = llx;
            this.lly = lly;
            this.urx = urx;
            this.ury = ury;
        }
        super.add(new PdfNumber(this.llx));
        super.add(new PdfNumber(this.lly));
        super.add(new PdfNumber(this.urx));
        super.add(new PdfNumber(this.ury));
    }

    public PdfRectangle(float llx, float lly, float urx, float ury) {
        this(llx, lly, urx, ury, 0);
    }


    
    public PdfRectangle(float urx, float ury, int rotation) {
        this(0, 0, urx, ury, rotation);
    }

    public PdfRectangle(float urx, float ury) {
        this(0, 0, urx, ury, 0);
    }
    

    
    public PdfRectangle(Rectangle rectangle, int rotation) {
        this(rectangle.getLeft(), rectangle.getBottom(), rectangle.getRight(), rectangle.getTop(), rotation);
    }
    
    public PdfRectangle(Rectangle rectangle) {
        this(rectangle.getLeft(), rectangle.getBottom(), rectangle.getRight(), rectangle.getTop(), 0);
    }
    
    
    
    public Rectangle getRectangle() {
        return new Rectangle(left(), bottom(), right(), top());
    }
    

    
    public boolean add(PdfObject object) {
        return false;
    }
    

    
    public float left() {
        return llx;
    }
    

    
    public float right() {
        return urx;
    }
    

    
    public float top() {
        return ury;
    }
    

    
    public float bottom() {
        return lly;
    }
    

    
    public float left(int margin) {
        return llx + margin;
    }
    

    
    public float right(int margin) {
        return urx - margin;
    }
    

    
    public float top(int margin) {
        return ury - margin;
    }
    

    
    public float bottom(int margin) {
        return lly + margin;
    }
    

    
    public float width() {
        return urx - llx;
    }
    

    
    public float height() {
        return ury - lly;
    }
    

    
    public PdfRectangle rotate() {
        return new PdfRectangle(lly, llx, ury, urx, 0);
    }
}