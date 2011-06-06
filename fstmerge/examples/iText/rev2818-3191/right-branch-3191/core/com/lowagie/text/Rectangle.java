

package com.lowagie.text;

import java.awt.Color;
import java.util.ArrayList;

import com.lowagie.text.pdf.GrayColor;



public class Rectangle implements Element {

    

    
    public static final int UNDEFINED = -1;

    
    public static final int TOP = 1;

    
    public static final int BOTTOM = 2;

    
    public static final int LEFT = 4;

    
    public static final int RIGHT = 8;

    
    public static final int NO_BORDER = 0;

    
    public static final int BOX = TOP + BOTTOM + LEFT + RIGHT;

    

    
    protected float llx;

    
    protected float lly;

    
    protected float urx;

    
    protected float ury;

    
    protected int rotation = 0;

    
    protected int border = UNDEFINED;

    
    protected float borderWidth = UNDEFINED;

    
    protected Color borderColor = null;

    
    protected Color backgroundColor = null;

    
    protected boolean useVariableBorders = false;

    
    protected float borderWidthLeft = UNDEFINED;

    
    protected float borderWidthRight = UNDEFINED;

    
    protected float borderWidthTop = UNDEFINED;

    
    protected float borderWidthBottom = UNDEFINED;

    
    protected Color borderColorLeft = null;

    
    protected Color borderColorRight = null;

    
    protected Color borderColorTop = null;

    
    protected Color borderColorBottom = null;

    

    
    public Rectangle(float llx, float lly, float urx, float ury) {
        this.llx = llx;
        this.lly = lly;
        this.urx = urx;
        this.ury = ury;
    }

    
    public Rectangle(float urx, float ury) {
        this(0, 0, urx, ury);
    }

    
    public Rectangle(Rectangle rect) {
        this(rect.llx, rect.lly, rect.urx, rect.ury);
        cloneNonPositionParameters(rect);
    }

    

    
    public boolean process(ElementListener listener) {
        try {
            return listener.add(this);
        } catch (DocumentException de) {
            return false;
        }
    }

    
    public int type() {
        return Element.RECTANGLE;
    }

    
    public ArrayList<Chunk> getChunks() {
        return new ArrayList<Chunk>();
    }
    
    
    public boolean isContent() {
        return true;
    }

    
    public boolean isNestable() {
        return false;
    }

    

    
    public void setLeft(float value) {
        llx = value;
    }

    
    public float getLeft() {
        return llx;
    }

    
    public float getLeft(float margin) {
        return llx + margin;
    }

    

    public void setRight(float value) {
        urx = value;
    }

    
    public float getRight() {
        return urx;
    }

    
    public float getRight(float margin) {
        return urx - margin;
    }

    
    public float getWidth() {
        return urx - llx;
    }

    
    public void setTop(float value) {
        ury = value;
    }

    
    public float getTop() {
        return ury;
    }

    
    public float getTop(float margin) {
        return ury - margin;
    }

    
    public void setBottom(float value) {
        lly = value;
    }

    
    public float getBottom() {
        return lly;
    }

    
    public float getBottom(float margin) {
        return lly + margin;
    }

    
    public float getHeight() {
        return ury - lly;
    }

    
    public void normalize() {
        if (llx > urx) {
            float a = llx;
            llx = urx;
            urx = a;
        }
        if (lly > ury) {
            float a = lly;
            lly = ury;
            ury = a;
        }
    }

    

    
    public int getRotation() {
        return rotation;
    }

    
    public Rectangle rotate() {
        Rectangle rect = new Rectangle(lly, llx, ury, urx);
        rect.rotation = rotation + 90;
        rect.rotation %= 360;
        return rect;
    }
    
    

    
    public int getBorder() {
        return border;
    }

    
    public boolean hasBorders() {
        return (border > 0)
                && ((borderWidth > 0) || (borderWidthLeft > 0)
                        || (borderWidthRight > 0) || (borderWidthTop > 0) || (borderWidthBottom > 0));
    }

    
    public boolean hasBorder(int type) {
        return border != UNDEFINED && (border & type) == type;
    }
    
    
    public void setBorder(int value) {
        border = value;
    }

    
    public void enableBorderSide(int side) {
        if (border == UNDEFINED) {
            border = 0;
        }
        border |= side;
    }

    
    public void disableBorderSide(int side) {
        if (border == UNDEFINED) {
            border = 0;
        }
        border &= ~side;
    }

    

    
    public float getBorderWidth() {
        return borderWidth;
    }
    
    

    public void setBorderWidth(float value) {
        borderWidth = value;
    }

    

    

    public Color getBorderColor() {
        return borderColor;
    }
    
    

    public void setBorderColor(Color value) {
        borderColor = value;
    }

    

    
    public Color getBackgroundColor() {
        return backgroundColor;
    }
    
    

    public void setBackgroundColor(Color value) {
        backgroundColor = value;
    }

    

    public float getGrayFill() {
        if (backgroundColor instanceof GrayColor)
            return ((GrayColor)backgroundColor).getGray();
        else
            return 0;
    }

    
    public void setGrayFill(float value) {
        backgroundColor = new GrayColor(value);
    }

    
    
    
    public boolean isUseVariableBorders() {
        return useVariableBorders;
    }

    
    public void setUseVariableBorders(boolean useVariableBorders) {
        this.useVariableBorders = useVariableBorders;
    }
    
    

    
    private float getVariableBorderWidth(float variableWidthValue, int side) {
        if ((border & side) != 0) {
            return variableWidthValue != UNDEFINED ? variableWidthValue
                    : borderWidth;
        } else {
            return 0;
        }
    }

    
    private void updateBorderBasedOnWidth(float width, int side) {
        useVariableBorders = true;
        if (width > 0) {
            enableBorderSide(side);
        } else {
            disableBorderSide(side);
        }
    }

    
    public float getBorderWidthLeft() {
        return getVariableBorderWidth(borderWidthLeft, LEFT);
    }

    
    public void setBorderWidthLeft(float borderWidthLeft) {
        this.borderWidthLeft = borderWidthLeft;
        updateBorderBasedOnWidth(borderWidthLeft, LEFT);
    }

    
    public float getBorderWidthRight() {
        return getVariableBorderWidth(borderWidthRight, RIGHT);
    }

    
    public void setBorderWidthRight(float borderWidthRight) {
        this.borderWidthRight = borderWidthRight;
        updateBorderBasedOnWidth(borderWidthRight, RIGHT);
    }

    
    public float getBorderWidthTop() {
        return getVariableBorderWidth(borderWidthTop, TOP);
    }

    
    public void setBorderWidthTop(float borderWidthTop) {
        this.borderWidthTop = borderWidthTop;
        updateBorderBasedOnWidth(borderWidthTop, TOP);
    }

    
    public float getBorderWidthBottom() {
        return getVariableBorderWidth(borderWidthBottom, BOTTOM);
    }

    
    public void setBorderWidthBottom(float borderWidthBottom) {
        this.borderWidthBottom = borderWidthBottom;
        updateBorderBasedOnWidth(borderWidthBottom, BOTTOM);
    }

    
    
    
    public Color getBorderColorLeft() {
        if (borderColorLeft == null) return borderColor;
        return borderColorLeft;
    }

    
    public void setBorderColorLeft(Color value) {
        borderColorLeft = value;
    }

    
    public Color getBorderColorRight() {
        if (borderColorRight == null) return borderColor;
        return borderColorRight;
    }

    
    public void setBorderColorRight(Color value) {
        borderColorRight = value;
    }

    
    public Color getBorderColorTop() {
        if (borderColorTop == null) return borderColor;
        return borderColorTop;
    }

    
    public void setBorderColorTop(Color value) {
        borderColorTop = value;
    }

    
    public Color getBorderColorBottom() {
        if (borderColorBottom == null) return borderColor;
        return borderColorBottom;
    }

    
    public void setBorderColorBottom(Color value) {
        borderColorBottom = value;
    }

    

    
    public Rectangle rectangle(float top, float bottom) {
        Rectangle tmp = new Rectangle(this);
        if (getTop() > top) {
            tmp.setTop(top);
            tmp.disableBorderSide(TOP);
        }
        if (getBottom() < bottom) {
            tmp.setBottom(bottom);
            tmp.disableBorderSide(BOTTOM);
        }
        return tmp;
    }

    
    public void cloneNonPositionParameters(Rectangle rect) {
        this.rotation = rect.rotation;
        this.border = rect.border;
        this.borderWidth = rect.borderWidth;
        this.borderColor = rect.borderColor;
        this.backgroundColor = rect.backgroundColor;
        this.useVariableBorders = rect.useVariableBorders;
        this.borderWidthLeft = rect.borderWidthLeft;
        this.borderWidthRight = rect.borderWidthRight;
        this.borderWidthTop = rect.borderWidthTop;
        this.borderWidthBottom = rect.borderWidthBottom;
        this.borderColorLeft = rect.borderColorLeft;
        this.borderColorRight = rect.borderColorRight;
        this.borderColorTop = rect.borderColorTop;
        this.borderColorBottom = rect.borderColorBottom;
    }

    
    public void softCloneNonPositionParameters(Rectangle rect) {
        if (rect.rotation != 0)
            this.rotation = rect.rotation;
        if (rect.border != UNDEFINED)
            this.border = rect.border;
        if (rect.borderWidth != UNDEFINED)
            this.borderWidth = rect.borderWidth;
        if (rect.borderColor != null)
            this.borderColor = rect.borderColor;
        if (rect.backgroundColor != null)
            this.backgroundColor = rect.backgroundColor;
        if (useVariableBorders)
            this.useVariableBorders = rect.useVariableBorders;
        if (rect.borderWidthLeft != UNDEFINED)
            this.borderWidthLeft = rect.borderWidthLeft;
        if (rect.borderWidthRight != UNDEFINED)
            this.borderWidthRight = rect.borderWidthRight;
        if (rect.borderWidthTop != UNDEFINED)
            this.borderWidthTop = rect.borderWidthTop;
        if (rect.borderWidthBottom != UNDEFINED)
            this.borderWidthBottom = rect.borderWidthBottom;
        if (rect.borderColorLeft != null)
            this.borderColorLeft = rect.borderColorLeft;
        if (rect.borderColorRight != null)
            this.borderColorRight = rect.borderColorRight;
        if (rect.borderColorTop != null)
            this.borderColorTop = rect.borderColorTop;
        if (rect.borderColorBottom != null)
            this.borderColorBottom = rect.borderColorBottom;
    }
    
    
    public String toString() {
        StringBuffer buf = new StringBuffer("Rectangle: ");
        buf.append(getWidth());
        buf.append('x');
        buf.append(getHeight());
        buf.append(" (rot: ");
        buf.append(rotation);
        buf.append(" degrees)");
        return buf.toString();
    }



    
    public float left() {
        return getLeft();
    }

    
    public float right() {
        return getRight();
    }

    
    public float top() {
        return getTop();
    }

    
    public float bottom() {
        return getBottom();
    }

    
    public float left(float margin) {
        return getLeft(margin);
    }

    
    public float right(float margin) {
        return getRight(margin);
    }

    
    public float width() {
        return getWidth();
    }

    
    public float top(float margin) {
        return getTop(margin);
    }

    
    public float bottom(float margin) {
        return getBottom(margin);
    }

    
    public float height() {
        return getHeight();
    }

    
    public int border() {
        return getBorder();
    }

    
    public float borderWidth() {
        return getBorderWidth();
    }

    
    
    public Color borderColor() {
        return getBorderColor();
    }

    
    public Color backgroundColor() {
        return getBackgroundColor();
    }

    
    
    public float grayFill() {
        return getGrayFill();
    }
    
}