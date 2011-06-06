

package com.lowagie.text.pdf.draw;

import com.lowagie.text.Element;
import com.lowagie.text.pdf.PdfContentByte;

import java.awt.Color;


public class LineSeparator extends VerticalPositionMark {
    
    
    protected float lineWidth = 1;
    
    protected float percentage = 100;
    
    protected Color lineColor;
    
    protected int alignment = Element.ALIGN_CENTER;
    
    
    public LineSeparator(float lineWidth, float percentage, Color lineColor, int align, float offset) {
        this.lineWidth = lineWidth;
        this.percentage = percentage;
        this.lineColor = lineColor;
        this.alignment = align;
        this.offset = offset;
    }

    
    public LineSeparator() {
    }

    
    public void draw(PdfContentByte canvas, float llx, float lly, float urx, float ury, float y) {
        canvas.saveState();
        drawLine(canvas, llx, urx, y);
        canvas.restoreState();
    }

    
    public void drawLine(PdfContentByte canvas, float leftX, float rightX, float y) {
        float w;
        if (getPercentage() < 0)
            w = -getPercentage();
        else
            w = (rightX - leftX) * getPercentage() / 100.0f;
        float s;
        switch (getAlignment()) {
            case Element.ALIGN_LEFT:
                s = 0;
                break;
            case Element.ALIGN_RIGHT:
                s = rightX - leftX - w;
                break;
            default:
                s = (rightX - leftX - w) / 2;
                break;
        }
        canvas.setLineWidth(getLineWidth());
        if (getLineColor() != null)
            canvas.setColorStroke(getLineColor());
        canvas.moveTo(s + leftX, y + offset);
        canvas.lineTo(s + w + leftX, y + offset);
        canvas.stroke();
    }
    
    
    public float getLineWidth() {
        return lineWidth;
    }

    
    public void setLineWidth(float lineWidth) {
        this.lineWidth = lineWidth;
    }

    
    public float getPercentage() {
        return percentage;
    }

    
    public void setPercentage(float percentage) {
        this.percentage = percentage;
    }

    
    public Color getLineColor() {
        return lineColor;
    }

    
    public void setLineColor(Color color) {
        this.lineColor = color;
    }

    
    public int getAlignment() {
        return alignment;
    }

    
    public void setAlignment(int align) {
        this.alignment = align;
    }
}