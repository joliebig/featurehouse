

package com.lowagie.text.pdf.draw;

import com.lowagie.text.pdf.PdfContentByte;


public class DottedLineSeparator extends LineSeparator {

    
    protected float gap = 5;
    
    
    public void draw(PdfContentByte canvas, float llx, float lly, float urx, float ury, float y) {
        canvas.saveState();
        canvas.setLineWidth(lineWidth);
        canvas.setLineCap(PdfContentByte.LINE_CAP_ROUND);
        canvas.setLineDash(0, gap, gap / 2);
        drawLine(canvas, llx, urx, y);
        canvas.restoreState();
    }

    
    public float getGap() {
        return gap;
    }

    
    public void setGap(float gap) {
        this.gap = gap;
    }

}