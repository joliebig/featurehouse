

package com.lowagie.text.rtf.table;

import java.awt.Color;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import com.lowagie.text.Rectangle;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.document.RtfDocument;



public class RtfBorderGroup extends RtfElement {
    
    private int borderType = RtfBorder.ROW_BORDER;
    
    private Hashtable borders = null;

    
    public RtfBorderGroup() {
        super(null);
        this.borders = new Hashtable();
    }
    
    
    public RtfBorderGroup(int bordersToAdd, int borderStyle, float borderWidth, Color borderColor) {
        super(null);
        this.borders = new Hashtable();
        addBorder(bordersToAdd, borderStyle, borderWidth, borderColor);
    }
    
    
    protected RtfBorderGroup(RtfDocument doc, int borderType, RtfBorderGroup borderGroup) {
        super(doc);
        this.borders = new Hashtable();
        this.borderType = borderType;
        if(borderGroup != null) {
            Iterator it = borderGroup.getBorders().entrySet().iterator();
            while(it.hasNext()) {
                Map.Entry entry = (Map.Entry) it.next();
                this.borders.put((Integer) entry.getKey(), new RtfBorder(this.document, this.borderType, (RtfBorder) entry.getValue()));
            }
        }
    }
    
    
    protected RtfBorderGroup(RtfDocument doc, int borderType, int bordersToUse, float borderWidth, Color borderColor) {
        super(doc);
        this.borderType = borderType;
        this.borders = new Hashtable();
        addBorder(bordersToUse, RtfBorder.BORDER_SINGLE, borderWidth, borderColor);
    }
    
    
    private void setBorder(int borderPosition, int borderStyle, float borderWidth, Color borderColor) {
        RtfBorder border = new RtfBorder(this.document, this.borderType, borderPosition, borderStyle, borderWidth, borderColor);
        this.borders.put(new Integer(borderPosition), border);
    }
    
    
    public void addBorder(int bordersToAdd, int borderStyle, float borderWidth, Color borderColor) {
        if((bordersToAdd & Rectangle.LEFT) == Rectangle.LEFT) {
            setBorder(RtfBorder.LEFT_BORDER, borderStyle, borderWidth, borderColor);
        }
        if((bordersToAdd & Rectangle.TOP) == Rectangle.TOP) {
            setBorder(RtfBorder.TOP_BORDER, borderStyle, borderWidth, borderColor);
        }
        if((bordersToAdd & Rectangle.RIGHT) == Rectangle.RIGHT) {
            setBorder(RtfBorder.RIGHT_BORDER, borderStyle, borderWidth, borderColor);
        }
        if((bordersToAdd & Rectangle.BOTTOM) == Rectangle.BOTTOM) {
            setBorder(RtfBorder.BOTTOM_BORDER, borderStyle, borderWidth, borderColor);
        }
        if((bordersToAdd & Rectangle.BOX) == Rectangle.BOX && this.borderType == RtfBorder.ROW_BORDER) {
            setBorder(RtfBorder.VERTICAL_BORDER, borderStyle, borderWidth, borderColor);
            setBorder(RtfBorder.HORIZONTAL_BORDER, borderStyle, borderWidth, borderColor);
        }
    }
    
    
    public void removeBorder(int bordersToRemove) {
        if((bordersToRemove & Rectangle.LEFT) == Rectangle.LEFT) {
            this.borders.remove(new Integer(RtfBorder.LEFT_BORDER));
        }
        if((bordersToRemove & Rectangle.TOP) == Rectangle.TOP) {
            this.borders.remove(new Integer(RtfBorder.TOP_BORDER));
        }
        if((bordersToRemove & Rectangle.RIGHT) == Rectangle.RIGHT) {
            this.borders.remove(new Integer(RtfBorder.RIGHT_BORDER));
        }
        if((bordersToRemove & Rectangle.BOTTOM) == Rectangle.BOTTOM) {
            this.borders.remove(new Integer(RtfBorder.BOTTOM_BORDER));
        }
        if((bordersToRemove & Rectangle.BOX) == Rectangle.BOX && this.borderType == RtfBorder.ROW_BORDER) {
            this.borders.remove(new Integer(RtfBorder.VERTICAL_BORDER));
            this.borders.remove(new Integer(RtfBorder.HORIZONTAL_BORDER));
        }
    }
    
        
    public void writeContent(final OutputStream result) throws IOException
    {
        Iterator it = this.borders.values().iterator();
        while(it.hasNext()) {
            ((RtfBorder) it.next()).writeContent(result);
        }
    }        
    
    
    protected Hashtable getBorders() {
        return this.borders;
    }
}
