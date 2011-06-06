

package com.lowagie.text.rtf.table;

import java.awt.Color;
import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.style.RtfColor;



public class RtfBorder extends RtfElement {

    
    protected static final byte[] ROW_BORDER_LEFT = "\\trbrdrl".getBytes();
    
    protected static final byte[] ROW_BORDER_TOP = "\\trbrdrt".getBytes();
    
    protected static final byte[] ROW_BORDER_RIGHT = "\\trbrdrr".getBytes();
    
    protected static final byte[] ROW_BORDER_BOTTOM = "\\trbrdrb".getBytes();
    
    protected static final byte[] ROW_BORDER_HORIZONTAL = "\\trbrdrh".getBytes();
    
    protected static final byte[] ROW_BORDER_VERTICAL = "\\trbrdrv".getBytes();
    
    protected static final byte[] CELL_BORDER_LEFT = "\\clbrdrl".getBytes();
    
    protected static final byte[] CELL_BORDER_TOP = "\\clbrdrt".getBytes();
    
    protected static final byte[] CELL_BORDER_RIGHT = "\\clbrdrr".getBytes();
    
    protected static final byte[] CELL_BORDER_BOTTOM = "\\clbrdrb".getBytes();
    
    protected static final byte[] BORDER_WIDTH = "\\brdrw".getBytes();
    
    protected static final byte[] BORDER_COLOR_NUMBER = "\\brdrcf".getBytes();
    
    protected static final byte[] BORDER_STYLE_SINGLE = "\\brdrs".getBytes();
    
    protected static final byte[] BORDER_STYLE_DOUBLE_THICK    = "\\brdrth".getBytes();
    
    protected static final byte[] BORDER_STYLE_SHADOWED = "\\brdrsh".getBytes();
    
    protected static final byte[] BORDER_STYLE_DOTTED = "\\brdrdot".getBytes();
    
    protected static final byte[] BORDER_STYLE_DASHED = "\\brdrdash".getBytes();
    
    protected static final byte[] BORDER_STYLE_HAIRLINE = "\\brdrhair".getBytes();
    
    protected static final byte[] BORDER_STYLE_DOUBLE = "\\brdrdb".getBytes();
    
    protected static final byte[] BORDER_STYLE_DOT_DASH = "\\brdrdashd".getBytes();
    
    protected static final byte[] BORDER_STYLE_DOT_DOT_DASH    = "\\brdrdashdd".getBytes();
    
    protected static final byte[] BORDER_STYLE_TRIPLE = "\\brdrtriple".getBytes();
    
    protected static final byte[] BORDER_STYLE_THICK_THIN = "\\brdrtnthsg".getBytes();
    
    protected static final byte[] BORDER_STYLE_THIN_THICK = "\\brdrthtnsg".getBytes();
    
    protected static final byte[] BORDER_STYLE_THIN_THICK_THIN = "\\brdrtnthtnsg".getBytes();
    
    protected static final byte[] BORDER_STYLE_THICK_THIN_MED = "\\brdrtnthmg".getBytes();
    
    protected static final byte[] BORDER_STYLE_THIN_THICK_MED = "\\brdrthtnmg".getBytes();
    
    protected static final byte[] BORDER_STYLE_THIN_THICK_THIN_MED = "\\brdrtnthtnmg".getBytes();
    
    protected static final byte[] BORDER_STYLE_THICK_THIN_LARGE = "\\brdrtnthlg".getBytes();
    
    protected static final byte[] BORDER_STYLE_THIN_THICK_LARGE    = "\\brdrthtnlg".getBytes();
    
    protected static final byte[] BORDER_STYLE_THIN_THICK_THIN_LARGE = "\\brdrtnthtnlg".getBytes();
    
    protected static final byte[] BORDER_STYLE_WAVY = "\\brdrwavy".getBytes();
    
    protected static final byte[] BORDER_STYLE_DOUBLE_WAVY = "\\brdrwavydb".getBytes();
    
    protected static final byte[] BORDER_STYLE_STRIPED = "\\brdrdashdotstr".getBytes();
    
    protected static final byte[] BORDER_STYLE_EMBOSS = "\\brdremboss".getBytes();
    
    protected static final byte[] BORDER_STYLE_ENGRAVE = "\\brdrengrave".getBytes();

    
    protected static final int ROW_BORDER = 1;
    
    protected static final int CELL_BORDER = 2;
    
    
    protected static final int NO_BORDER = 0;
    
    protected static final int LEFT_BORDER = 1;
    
    protected static final int TOP_BORDER = 2;
    
    protected static final int RIGHT_BORDER = 4;
    
    protected static final int BOTTOM_BORDER = 8;
    
    protected static final int BOX_BORDER = 15;
    
    protected static final int VERTICAL_BORDER = 16;
    
    protected static final int HORIZONTAL_BORDER = 32;
    
    
    public static final int BORDER_NONE = 0;
    
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
    
    
    private int borderType = ROW_BORDER;
    
    private int borderPosition = NO_BORDER;
    
    private int borderStyle = BORDER_NONE;
    
    private int borderWidth = 20;
    
    private RtfColor borderColor = null;
    
    
    protected RtfBorder(RtfDocument doc, int borderType, RtfBorder border) {
        super(doc);
        this.borderType = borderType;
        this.borderPosition = border.getBorderPosition();
        this.borderStyle = border.getBorderStyle();
        this.borderWidth = border.getBorderWidth();
        this.borderColor = new RtfColor(this.document, border.getBorderColor());
    }
    
    
    protected RtfBorder(RtfDocument doc, int borderType, int borderPosition, int borderStyle, float borderWidth, Color borderColor) {
        super(doc);
        this.borderType = borderType;
        this.borderPosition = borderPosition;
        this.borderStyle = borderStyle;
        this.borderWidth = (int) Math.min((borderWidth * TWIPS_FACTOR), 75);
        if(this.borderWidth == 0) {
            this.borderStyle = BORDER_NONE;
        }
        if(borderColor == null) {
            this.borderColor = new RtfColor(this.document, new Color(0, 0, 0));
        } else {
            this.borderColor = new RtfColor(this.document, borderColor);
        }
    }
    
    
    public void writeContent(final OutputStream result) throws IOException
    {
        if(this.borderStyle == BORDER_NONE || this.borderPosition == NO_BORDER || this.borderWidth == 0) {
            return;
        }

        if(this.borderType == ROW_BORDER) {
            switch(this.borderPosition) {
                case LEFT_BORDER:
                    result.write(ROW_BORDER_LEFT);
                    break;
                case TOP_BORDER:
                    result.write(ROW_BORDER_TOP);
                    break;
                case RIGHT_BORDER:
                    result.write(ROW_BORDER_RIGHT);
                    break;
                case BOTTOM_BORDER:
                    result.write(ROW_BORDER_BOTTOM);
                    break;
                case HORIZONTAL_BORDER:
                    result.write(ROW_BORDER_HORIZONTAL);
                    break;
                case VERTICAL_BORDER:
                    result.write(ROW_BORDER_VERTICAL);
                    break;
                default:
                    return;
            }
            result.write(writeBorderStyle());
            result.write(BORDER_WIDTH);
            result.write(intToByteArray(this.borderWidth));
            result.write(BORDER_COLOR_NUMBER);
            result.write(intToByteArray(this.borderColor.getColorNumber()));
            result.write('\n');
        } else if(this.borderType == CELL_BORDER) {
            switch(this.borderPosition) {
                case LEFT_BORDER:
                    result.write(CELL_BORDER_LEFT);
                    break;
                case TOP_BORDER:
                    result.write(CELL_BORDER_TOP);
                    break;
                case RIGHT_BORDER:
                    result.write(CELL_BORDER_RIGHT);
                    break;
                case BOTTOM_BORDER:
                    result.write(CELL_BORDER_BOTTOM);
                    break;
                default:
                    return;
            }
            result.write(writeBorderStyle());
            result.write(BORDER_WIDTH);
            result.write(intToByteArray(this.borderWidth));
            result.write(BORDER_COLOR_NUMBER);
            result.write(intToByteArray(this.borderColor.getColorNumber()));
            result.write('\n');
        }        
    }
     
    
    private byte[] writeBorderStyle() {
        switch(this.borderStyle) {
            case BORDER_NONE                    : return new byte[0];
            case BORDER_SINGLE                     : return BORDER_STYLE_SINGLE;
            case BORDER_DOUBLE_THICK             : return BORDER_STYLE_DOUBLE_THICK;
            case BORDER_SHADOWED                 : return BORDER_STYLE_SHADOWED;
            case BORDER_DOTTED                   : return BORDER_STYLE_DOTTED;
            case BORDER_DASHED                   : return BORDER_STYLE_DASHED;
            case BORDER_HAIRLINE                   : return BORDER_STYLE_HAIRLINE;
            case BORDER_DOUBLE                       : return BORDER_STYLE_DOUBLE;
            case BORDER_DOT_DASH                   : return BORDER_STYLE_DOT_DASH;
            case BORDER_DOT_DOT_DASH            : return BORDER_STYLE_DOT_DOT_DASH;
            case BORDER_TRIPLE                    : return BORDER_STYLE_TRIPLE;
            case BORDER_THICK_THIN                : return BORDER_STYLE_THICK_THIN;
            case BORDER_THIN_THICK                : return BORDER_STYLE_THIN_THICK;
            case BORDER_THIN_THICK_THIN            : return BORDER_STYLE_THIN_THICK_THIN;
            case BORDER_THICK_THIN_MED            : return BORDER_STYLE_THICK_THIN_MED;
            case BORDER_THIN_THICK_MED            : return BORDER_STYLE_THIN_THICK_MED;
            case BORDER_THIN_THICK_THIN_MED        : return BORDER_STYLE_THIN_THICK_THIN_MED;
            case BORDER_THICK_THIN_LARGE        : return BORDER_STYLE_THICK_THIN_LARGE;
            case BORDER_THIN_THICK_LARGE        : return BORDER_STYLE_THIN_THICK_LARGE;
            case BORDER_THIN_THICK_THIN_LARGE    : return BORDER_STYLE_THIN_THICK_THIN_LARGE;
            case BORDER_WAVY                    : return BORDER_STYLE_WAVY;
            case BORDER_DOUBLE_WAVY                : return BORDER_STYLE_DOUBLE_WAVY;
            case BORDER_STRIPED                    : return BORDER_STYLE_STRIPED;
            case BORDER_EMBOSS                    : return BORDER_STYLE_EMBOSS;
            case BORDER_ENGRAVE                    : return BORDER_STYLE_ENGRAVE;
            default                             : return BORDER_STYLE_SINGLE;
        }
    }
    
    
    protected RtfColor getBorderColor() {
        return borderColor;
    }

    
    protected int getBorderPosition() {
        return borderPosition;
    }

    
    protected int getBorderStyle() {
        return borderStyle;
    }

    
    protected int getBorderType() {
        return borderType;
    }

    
    protected int getBorderWidth() {
        return borderWidth;
    }
}
