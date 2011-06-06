package com.lowagie.text.rtf.graphic;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

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

    
    public byte[] write() 
    {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            writeContent(result);
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }
    
    public void writeContent(final OutputStream result) throws IOException
    {        
        result.write("\\shpleft".getBytes());
        result.write(intToByteArray(this.left));
        result.write("\\shptop".getBytes());
        result.write(intToByteArray(this.top));
        result.write("\\shpright".getBytes());
        result.write(intToByteArray(this.right));
        result.write("\\shpbottom".getBytes());
        result.write(intToByteArray(this.bottom));
        result.write("\\shpz".getBytes());
        result.write(intToByteArray(this.zOrder));
        switch(this.xRelativePos) {
        case POSITION_X_RELATIVE_PAGE: result.write("\\shpbxpage".getBytes()); break;
        case POSITION_X_RELATIVE_MARGIN: result.write("\\shpbxmargin".getBytes()); break;
        case POSITION_X_RELATIVE_COLUMN: result.write("\\shpbxcolumn".getBytes()); break;
        }
        if(this.ignoreXRelative) {
            result.write("\\shpbxignore".getBytes());
        }
        switch(this.yRelativePos) {
        case POSITION_Y_RELATIVE_PAGE: result.write("\\shpbypage".getBytes()); break;
        case POSITION_Y_RELATIVE_MARGIN: result.write("\\shpbymargin".getBytes()); break;
        case POSITION_Y_RELATIVE_PARAGRAPH: result.write("\\shpbypara".getBytes()); break;
        }
        if(this.ignoreYRelative) {
            result.write("\\shpbyignore".getBytes());
        }
        if(this.shapeBelowText) {
            result.write("\\shpfblwtxt1".getBytes());
        } else {
            result.write("\\shpfblwtxt0".getBytes());
        }
    }

}
