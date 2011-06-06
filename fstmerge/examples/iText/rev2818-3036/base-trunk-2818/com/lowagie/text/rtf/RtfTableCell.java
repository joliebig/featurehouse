

package com.lowagie.text.rtf;

import java.util.Properties;

import com.lowagie.text.BadElementException;
import com.lowagie.text.Cell;
import com.lowagie.text.Element;


public class RtfTableCell extends Cell
{
    
    
    
    public static final int BORDER_UNDEFINED = 0;
    
    
    public static final int BORDER_SINGLE = 1;
    
    
    public static final int BORDER_DOUBLE_THICK = 2;
    
    
    public static final int BORDER_SHADOWED = 3;
    
    
    public static final int BORDER_DOTTED = 4;
    
    
    public static final int BORDER_DASHED = 5;
    
    
    public static final int BORDER_HAIRLINE = 6;
    
    
    public static final int BORDER_DOUBLE = 7;
    
    
    public static final int BORDER_DOT_DASH = 8;
    
    
    public static final int BORDER_DOT_DOT_DASH = 9;
    
    
    public static final int BORDER_TRIPLE = 10;

    
    public static final int BORDER_THICK_THIN = 11;
    
    
    public static final int BORDER_THIN_THICK = 12;
    
    
    public static final int BORDER_THIN_THICK_THIN = 13;
    
    
    public static final int BORDER_THICK_THIN_MED = 14;
    
    
    public static final int BORDER_THIN_THICK_MED = 15;
    
    
    public static final int BORDER_THIN_THICK_THIN_MED = 16;
    
    
    public static final int BORDER_THICK_THIN_LARGE = 17;
    
    
    public static final int BORDER_THIN_THICK_LARGE = 18;
    
    
    public static final int BORDER_THIN_THICK_THIN_LARGE = 19;
    
    
    public static final int BORDER_WAVY = 20;
    
    
    public static final int BORDER_DOUBLE_WAVY = 21;
    
    
    public static final int BORDER_STRIPED = 22;
    
    
    public static final int BORDER_EMBOSS = 23;
    
    
    public static final int BORDER_ENGRAVE = 24;
    
    
    private float topBorderWidth;
    private float leftBorderWidth;
    private float rightBorderWidth;
    private float bottomBorderWidth;
    private int topBorderStyle = 1;
    private int leftBorderStyle = 1;
    private int rightBorderStyle = 1;
    private int bottomBorderStyle = 1;
    


    public RtfTableCell(boolean dummy) {
        super(dummy);
    }
    

    public RtfTableCell(Element element) throws BadElementException {
        super(element);
    }
    

    public RtfTableCell(String content) {
        super(content);
    }
    


    public RtfTableCell(Properties attributes) {
        super(attributes);
    }
    
    
    public void setBorderWidth(float f) {
        super.setBorderWidth(f);
        topBorderWidth = f;
        leftBorderWidth = f;
        rightBorderWidth = f;
        bottomBorderWidth = f;
    }
    
    
    public void setTopBorderWidth(float f) {
        topBorderWidth = f;
    }
    
    
    public float topBorderWidth() {
        return topBorderWidth;
    }
    
    
    public void setLeftBorderWidth(float f) {
        leftBorderWidth = f;
    }
    
    
    public float leftBorderWidth() {
        return leftBorderWidth;
    }
    
    
    public void setRightBorderWidth(float f) {
        rightBorderWidth = f;
    }
    
    
    public float rightBorderWidth() {
        return rightBorderWidth;
    }
    
    
    public void setBottomBorderWidth(float f) {
        bottomBorderWidth = f;
    }
    
    
    public float bottomBorderWidth() {
        return bottomBorderWidth;
    }
    
    
    public void setBorderStyle(int style) {
        topBorderStyle = style;
        leftBorderStyle = style;
        rightBorderStyle = style;
        bottomBorderStyle = style;
    }
    
    
    public void setTopBorderStyle(int style) {
        topBorderStyle = style;
    }
    
    
    public int topBorderStyle() {
        return topBorderStyle;
    }
    
    
    public void setLeftBorderStyle(int style) {
        leftBorderStyle = style;
    }
    
    
    public int leftBorderStyle() {
        return leftBorderStyle;
    }
    
    
    public void setRightBorderStyle(int style) {
        rightBorderStyle = style;
    }
    
    
    public int rightBorderStyle() {
        return rightBorderStyle;
    }
    
    
    public void setBottomBorderStyle(int style) {
        bottomBorderStyle = style;
    }
    
    
    public int bottomBorderStyle() {
        return bottomBorderStyle;
    }
    
    
    protected static byte[] getStyleControlWord(int style) {
        switch(style)
        {
            case BORDER_UNDEFINED                : return "brdrs".getBytes();
            case BORDER_SINGLE                     : return "brdrs".getBytes();
            case BORDER_DOUBLE_THICK             : return "brdrth".getBytes();
            case BORDER_SHADOWED                 : return "brdrsh".getBytes();
            case BORDER_DOTTED                   : return "brdrdot".getBytes();
            case BORDER_DASHED                   : return "brdrdash".getBytes();
            case BORDER_HAIRLINE                   : return "brdrhair".getBytes();
            case BORDER_DOUBLE                       : return "brdrdb".getBytes();
            case BORDER_DOT_DASH                   : return "brdrdashd".getBytes();
            case BORDER_DOT_DOT_DASH            : return "brdrdashdd".getBytes();
            case BORDER_TRIPLE                    : return "brdrtriple".getBytes();
            case BORDER_THICK_THIN                : return "brdrtnthsg".getBytes();
            case BORDER_THIN_THICK                : return "brdrthtnsg".getBytes();
            case BORDER_THIN_THICK_THIN            : return "brdrtnthtnsg".getBytes();
            case BORDER_THICK_THIN_MED            : return "brdrtnthmg".getBytes();
            case BORDER_THIN_THICK_MED            : return "brdrthtnmg".getBytes();
            case BORDER_THIN_THICK_THIN_MED        : return "brdrtnthtnmg".getBytes();
            case BORDER_THICK_THIN_LARGE        : return "brdrtnthlg".getBytes();
            case BORDER_THIN_THICK_LARGE        : return "brdrthtnlg".getBytes();
            case BORDER_THIN_THICK_THIN_LARGE    : return "brdrtnthtnlg".getBytes();
            case BORDER_WAVY                    : return "brdrwavy".getBytes();
            case BORDER_DOUBLE_WAVY                : return "brdrwavydb".getBytes();
            case BORDER_STRIPED                    : return "brdrdashdotstr".getBytes();
            case BORDER_EMBOSS                    : return "brdremboss".getBytes();
            case BORDER_ENGRAVE                    : return "brdrengrave".getBytes();
        }
        
        return "brdrs".getBytes();
    }
}
