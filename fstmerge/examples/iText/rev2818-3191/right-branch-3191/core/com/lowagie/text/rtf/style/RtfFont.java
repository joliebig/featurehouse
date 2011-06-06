

package com.lowagie.text.rtf.style;

import java.awt.Color;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Font;
import com.lowagie.text.rtf.RtfExtendedElement;
import com.lowagie.text.rtf.document.RtfDocument;


public class RtfFont extends Font implements RtfExtendedElement {
    
    private static final byte[] FONT_FAMILY = "\\froman".getBytes();
    
    private static final byte[] FONT_CHARSET = "\\fcharset".getBytes();
    
    public static final byte[] FONT_SIZE = "\\fs".getBytes();
    
    private static final byte[] FONT_BOLD = "\\b".getBytes();
    
    private static final byte[] FONT_ITALIC = "\\i".getBytes();
    
    private static final byte[] FONT_UNDERLINE = "\\ul".getBytes();
    
    private static final byte[] FONT_STRIKETHROUGH = "\\strike".getBytes();
    
    private static final byte[] FONT_DOUBLE_STRIKETHROUGH = "\\striked".getBytes();
    
    private static final byte[] FONT_SHADOW = "\\shad".getBytes();
    
    private static final byte[] FONT_OUTLINE = "\\outl".getBytes();
    
    private static final byte[] FONT_EMBOSSED = "\\embo".getBytes();
    
    private static final byte[] FONT_ENGRAVED = "\\impr".getBytes();
    
    private static final byte[] FONT_HIDDEN = "\\v".getBytes();
    
    
    public static final int STYLE_NONE = 0;
    
    public static final int STYLE_BOLD = 1;
    
    public static final int STYLE_ITALIC = 2;
    
    public static final int STYLE_UNDERLINE = 4;
    
    public static final int STYLE_STRIKETHROUGH = 8;
    
    public static final int STYLE_DOUBLE_STRIKETHROUGH = 16;
    
    public static final int STYLE_SHADOW = 32;
    
    public static final int STYLE_OUTLINE = 64;
    
    public static final int STYLE_EMBOSSED = 128;
    
    public static final int STYLE_ENGRAVED = 256;
    
    public static final int STYLE_HIDDEN = 512;

    
    private String fontName = "Times New Roman";
    
    private int fontSize = 10;
    
    private int fontStyle = STYLE_NONE;
    
    private int fontNumber = 0;
    
    private RtfColor color = null;
    
    private int charset = 0;
    
    protected RtfDocument document = null;
    
    
    public RtfFont(String fontName) {
        super(Font.UNDEFINED, Font.UNDEFINED, Font.UNDEFINED, null);
        this.fontName = fontName;
    }
    
    
    public RtfFont(String fontName, float size) {
        super(Font.UNDEFINED, size, Font.UNDEFINED, null);
        this.fontName = fontName;
    }
    
    
    public RtfFont(String fontName, float size, int style) {
        super(Font.UNDEFINED, size, style, null);
        this.fontName = fontName;
    }
    
    
    public RtfFont(String fontName, float size, int style, Color color) {
        super(Font.UNDEFINED, size, style, color);
        this.fontName = fontName;
    }
    
    
    public RtfFont(String fontName, float size, int style, Color color, int charset) {
        this(fontName, size, style, color);
        this.charset = charset;
    }

    
    protected RtfFont(RtfDocument doc, int fontNumber) {
        this.document = doc;
        this.fontNumber = fontNumber;
        color = new RtfColor(doc, 0, 0, 0);
    }

    
    public RtfFont(RtfDocument doc, Font font) {
        this.document = doc;
        if(font != null) {
            if(font instanceof RtfFont) {
                this.fontName = ((RtfFont) font).getFontName();
                this.charset = ((RtfFont) font).getCharset();
            } else {
                setToDefaultFamily(font.getFamilyname());
            }
            if(font.getBaseFont() != null) {
                String[][] fontNames = font.getBaseFont().getFullFontName();
                for(int i = 0; i < fontNames.length; i++) {
                    if(fontNames[i][2].equals("0")) {
                        this.fontName = fontNames[i][3];
                        break;
                    } else if(fontNames[i][2].equals("1033") || fontNames[i][2].equals("")) {
                        this.fontName = fontNames[i][3];
                    }
                }
            }
            
            setSize(font.getSize());
            setStyle(font.getStyle());
            setColor(font.getColor());
        }

        if(this.fontName.equalsIgnoreCase("unknown")) {
            return;
        }

        if(document != null) {
            setRtfDocument(document);
        }
    }

    
    public byte[] writeDefinition() {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            writeDefinition(result);
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }
    
    
    public void writeDefinition(final OutputStream result) throws IOException
    {
        result.write(FONT_FAMILY);
        result.write(FONT_CHARSET);
        result.write(intToByteArray(charset));
        result.write(DELIMITER);
        
        document.filterSpecialChar(result, fontName, true, false);
    }
    
    
    public byte[] writeBegin() {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            if(this.fontNumber != Font.UNDEFINED) {
                result.write(RtfFontList.FONT_NUMBER);
                result.write(intToByteArray(fontNumber));
            }
            if(this.fontSize != Font.UNDEFINED) {
                result.write(FONT_SIZE);
                result.write(intToByteArray(fontSize * 2));
            }
            if(this.fontStyle != UNDEFINED) {
                if((fontStyle & STYLE_BOLD) == STYLE_BOLD) {
                    result.write(FONT_BOLD);
                }
                if((fontStyle & STYLE_ITALIC) == STYLE_ITALIC) {
                    result.write(FONT_ITALIC);
                }
                if((fontStyle & STYLE_UNDERLINE) == STYLE_UNDERLINE) {
                    result.write(FONT_UNDERLINE);
                }
                if((fontStyle & STYLE_STRIKETHROUGH) == STYLE_STRIKETHROUGH) {
                    result.write(FONT_STRIKETHROUGH);
                }
                if((fontStyle & STYLE_HIDDEN) == STYLE_HIDDEN) {
                    result.write(FONT_HIDDEN);
                }
                if((fontStyle & STYLE_DOUBLE_STRIKETHROUGH) == STYLE_DOUBLE_STRIKETHROUGH) {
                    result.write(FONT_DOUBLE_STRIKETHROUGH);
                    result.write(intToByteArray(1));
                }
                if((fontStyle & STYLE_SHADOW) == STYLE_SHADOW) {
                    result.write(FONT_SHADOW);
                }
                if((fontStyle & STYLE_OUTLINE) == STYLE_OUTLINE) {
                    result.write(FONT_OUTLINE);
                }
                if((fontStyle & STYLE_EMBOSSED) == STYLE_EMBOSSED) {
                    result.write(FONT_EMBOSSED);
                }
                if((fontStyle & STYLE_ENGRAVED) == STYLE_ENGRAVED) {
                    result.write(FONT_ENGRAVED);
                }
            }
            if(color != null) {
                result.write(color.writeBegin());
            }
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }
    
    
    public byte[] writeEnd() {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            if(this.fontStyle != UNDEFINED) {
                if((fontStyle & STYLE_BOLD) == STYLE_BOLD) {
                    result.write(FONT_BOLD);
                    result.write(intToByteArray(0));
                }
                if((fontStyle & STYLE_ITALIC) == STYLE_ITALIC) {
                    result.write(FONT_ITALIC);
                    result.write(intToByteArray(0));
                }
                if((fontStyle & STYLE_UNDERLINE) == STYLE_UNDERLINE) {
                    result.write(FONT_UNDERLINE);
                    result.write(intToByteArray(0));
                }
                if((fontStyle & STYLE_STRIKETHROUGH) == STYLE_STRIKETHROUGH) {
                    result.write(FONT_STRIKETHROUGH);
                    result.write(intToByteArray(0));
                }
                if((fontStyle & STYLE_HIDDEN) == STYLE_HIDDEN) {
                    result.write(FONT_HIDDEN);
                    result.write(intToByteArray(0));
                }
                if((fontStyle & STYLE_DOUBLE_STRIKETHROUGH) == STYLE_DOUBLE_STRIKETHROUGH) {
                    result.write(FONT_DOUBLE_STRIKETHROUGH);
                    result.write(intToByteArray(0));
                }
                if((fontStyle & STYLE_SHADOW) == STYLE_SHADOW) {
                    result.write(FONT_SHADOW);
                    result.write(intToByteArray(0));
                }
                if((fontStyle & STYLE_OUTLINE) == STYLE_OUTLINE) {
                    result.write(FONT_OUTLINE);
                    result.write(intToByteArray(0));
                }
                if((fontStyle & STYLE_EMBOSSED) == STYLE_EMBOSSED) {
                    result.write(FONT_EMBOSSED);
                    result.write(intToByteArray(0));
                }
                if((fontStyle & STYLE_ENGRAVED) == STYLE_ENGRAVED) {
                    result.write(FONT_ENGRAVED);
                    result.write(intToByteArray(0));
                }
            }
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }

    
    public byte[] write() {
        return new byte[0];
    }
    
    public void writeContent(OutputStream out) throws IOException
    {        
    }
    
    
    public boolean equals(Object obj) {
        if(!(obj instanceof RtfFont)) {
            return false;
        }
        RtfFont font = (RtfFont) obj;
        boolean result = true;
        result = result & this.fontName.equals(font.getFontName());

        return result;
    }

    
    public int hashCode() {
        return (this.fontName + this.fontSize + "-" + this.fontStyle).hashCode();
    }
    
    
    public String getFontName() {
        return this.fontName;
    }

    
    protected void setFontName(String fontName) {
        this.fontName = fontName;
        if(document != null) {
            this.fontNumber = document.getDocumentHeader().getFontNumber(this);
        }
    }
    
    
    public String getFamilyname() {
        return this.fontName;
    }
    
    
    public void setFamily(String family){
        super.setFamily(family);
        setToDefaultFamily(family);
    }
    
    
    private void setToDefaultFamily(String familyname){
        switch (Font.getFamilyIndex(familyname)) {
            case Font.COURIER:
                this.fontName = "Courier";
                break;
            case Font.HELVETICA:
                this.fontName = "Arial";
                break;
            case Font.SYMBOL:
                this.fontName = "Symbol";
                this.charset = 2;
                break;
            case Font.TIMES_ROMAN:
                this.fontName = "Times New Roman";
                break;
            case Font.ZAPFDINGBATS:
                this.fontName = "Windings";
                break;
            default:
                this.fontName = familyname;
        }
    }
    
    
    public int getFontSize() {
        return this.fontSize;
    }
    
    
    public void setSize(float size){
        super.setSize(size);
        this.fontSize = (int) getSize();
    }

    
    public int getFontStyle() {
        return this.fontStyle;
    }
    
    
    public void setStyle(int style){
        super.setStyle(style);
        this.fontStyle = getStyle();
    }
    
    
    public void setStyle(String style) {
        super.setStyle(style);
        fontStyle = getStyle();
    }

    
    public int getCharset() {
        return charset;
    }

    
    public void setCharset(int charset) {
        this.charset = charset;
    }

    
    public int getFontNumber() {
        return fontNumber;
    }

    
    public void setRtfDocument(RtfDocument doc) {
        this.document = doc;
        if(document != null) {
            this.fontNumber = document.getDocumentHeader().getFontNumber(this);
        }
        if(this.color != null) {
            this.color.setRtfDocument(this.document);
        }
    }

    
    public void setInTable(boolean inTable) {
    }
    
    
    public void setInHeader(boolean inHeader) {
    }
    
    
    public void setColor(Color color) {
        super.setColor(color);
        if(color != null) {
            this.color = new RtfColor(document, color);
        } else {
            this.color = null;
        }
    }
    
    
    public void setColor(int red, int green, int blue) {
        super.setColor(red,green,blue);
        this.color = new RtfColor(document, red, green, blue);
    }

    
    protected byte[] intToByteArray(int i) {
        return Integer.toString(i).getBytes();
    }

    
    public Font difference(Font font) {
        String dFamilyname = font.getFamilyname();
        if(dFamilyname == null || dFamilyname.trim().equals("") || dFamilyname.trim().equalsIgnoreCase("unknown")) {
            dFamilyname = this.fontName;
        }

        float dSize = font.getSize();
        if(dSize == Font.UNDEFINED) {
            dSize = this.getSize();
        }

        int dStyle = Font.UNDEFINED;
        if(this.getStyle() != Font.UNDEFINED && font.getStyle() != Font.UNDEFINED) {
            dStyle = this.getStyle() | font.getStyle();
        } else if(this.getStyle() != Font.UNDEFINED) {
            dStyle = this.getStyle();
        } else if(font.getStyle() != Font.UNDEFINED) {
            dStyle = font.getStyle();
        }

        Color dColor = font.getColor();
        if(dColor == null) {
            dColor = this.getColor();
        }
        
        int dCharset = this.charset;
        if(font instanceof RtfFont) {
            dCharset = ((RtfFont) font).getCharset();
        }
        
        return new RtfFont(dFamilyname, dSize, dStyle, dColor, dCharset);
    }
}
