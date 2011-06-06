

package com.lowagie.text.pdf.draw;

import java.util.ArrayList;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ElementListener;
import com.lowagie.text.pdf.PdfContentByte;



public class VerticalPositionMark implements DrawInterface, Element {

    
    protected DrawInterface drawInterface = null;

    
    protected float offset = 0;
    
    
    public VerticalPositionMark() {    
    }

    
    public VerticalPositionMark(DrawInterface drawInterface, float offset) {
        this.drawInterface = drawInterface;
        this.offset = offset;
    }
    
    
    public void draw(PdfContentByte canvas, float llx, float lly, float urx, float ury, float y) {
        if (drawInterface != null) {
            drawInterface.draw(canvas, llx, lly, urx, ury, y + offset);
        }
    }
    
    
    public boolean process(ElementListener listener) {
        try {
            return listener.add(this);
        } catch (DocumentException e) {
            return false;
        }
    }

    
    public int type() {
        return Element.YMARK;
    }

    
    public boolean isContent() {
        return true;
    }

    
    public boolean isNestable() {
        return false;
    }

    
    public ArrayList getChunks() {
        ArrayList list = new ArrayList();
        list.add(new Chunk(this, true));
        return list;
    }

    
    public DrawInterface getDrawInterface() {
        return drawInterface;
    }

    
    public void setDrawInterface(DrawInterface drawInterface) {
        this.drawInterface = drawInterface;
    }

    
    public float getOffset() {
        return offset;
    }

    
    public void setOffset(float offset) {
        this.offset = offset;
    }
}
