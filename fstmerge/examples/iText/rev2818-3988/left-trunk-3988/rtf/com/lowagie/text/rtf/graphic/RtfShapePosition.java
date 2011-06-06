package com.lowagie.text.rtf.graphic;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.DocWriter;
import com.lowagie.text.rtf.RtfAddableElement;


public class RtfShapePosition extends RtfAddableElement {
    
    public static final int POSITION_X_RELATIVE_PAGE = 0;
    
    public static final int POSITION_X_RELATIVE_MARGIN = 1;
    
    public static final int POSITION_X_RELATIVE_COLUMN = 2;
    
    public static final int POSITION_Y_RELATIVE_PAGE = 0;
    
    public static final int POSITION_Y_RELATIVE_MARGIN = 1;
    
    public static final int POSITION_Y_RELATIVE_PARAGRAPH = 2;
    
    
    private int top = 0;
    
    private int left = 0;
    
    private int right = 0;
    
    private int bottom = 0;
    
    private int zOrder = 0;
    
    private int xRelativePos = POSITION_X_RELATIVE_PAGE;
    
    private int yRelativePos = POSITION_Y_RELATIVE_PAGE;
    
    private boolean ignoreXRelative = false;
    
    private boolean ignoreYRelative = false;
    
    private boolean shapeBelowText = false;

    
    public RtfShapePosition(int top, int left, int right, int bottom) {
        this.top = top;
        this.left = left;
        this.right = right;
        this.bottom = bottom;
    }
    
    
    public boolean isShapeBelowText() {
        return shapeBelowText;
    }

    
    public void setShapeBelowText(boolean shapeBelowText) {
        this.shapeBelowText = shapeBelowText;
    }

    
    public void setXRelativePos(int relativePos) {
        xRelativePos = relativePos;
    }

    
    public void setYRelativePos(int relativePos) {
        yRelativePos = relativePos;
    }

    
    public void setZOrder(int order) {
        zOrder = order;
    }

    
    protected void setIgnoreXRelative(boolean ignoreXRelative) {
        this.ignoreXRelative = ignoreXRelative;
    }

    
    protected void setIgnoreYRelative(boolean ignoreYRelative) {
        this.ignoreYRelative = ignoreYRelative;
    }

    
    public void writeContent(final OutputStream result) throws IOException
    {        
        result.write(DocWriter.getISOBytes("\\shpleft"));
        result.write(intToByteArray(this.left));
        result.write(DocWriter.getISOBytes("\\shptop"));
        result.write(intToByteArray(this.top));
        result.write(DocWriter.getISOBytes("\\shpright"));
        result.write(intToByteArray(this.right));
        result.write(DocWriter.getISOBytes("\\shpbottom"));
        result.write(intToByteArray(this.bottom));
        result.write(DocWriter.getISOBytes("\\shpz"));
        result.write(intToByteArray(this.zOrder));
        switch(this.xRelativePos) {
        case POSITION_X_RELATIVE_PAGE: result.write(DocWriter.getISOBytes("\\shpbxpage")); break;
        case POSITION_X_RELATIVE_MARGIN: result.write(DocWriter.getISOBytes("\\shpbxmargin")); break;
        case POSITION_X_RELATIVE_COLUMN: result.write(DocWriter.getISOBytes("\\shpbxcolumn")); break;
        }
        if(this.ignoreXRelative) {
            result.write(DocWriter.getISOBytes("\\shpbxignore"));
        }
        switch(this.yRelativePos) {
        case POSITION_Y_RELATIVE_PAGE: result.write(DocWriter.getISOBytes("\\shpbypage")); break;
        case POSITION_Y_RELATIVE_MARGIN: result.write(DocWriter.getISOBytes("\\shpbymargin")); break;
        case POSITION_Y_RELATIVE_PARAGRAPH: result.write(DocWriter.getISOBytes("\\shpbypara")); break;
        }
        if(this.ignoreYRelative) {
            result.write(DocWriter.getISOBytes("\\shpbyignore"));
        }
        if(this.shapeBelowText) {
            result.write(DocWriter.getISOBytes("\\shpfblwtxt1"));
        } else {
            result.write(DocWriter.getISOBytes("\\shpfblwtxt0"));
        }
    }

}
