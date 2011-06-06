

package com.lowagie.text;

import java.awt.Color;

import com.lowagie.text.html.Markup;
import com.lowagie.text.pdf.BaseFont;



public class Font implements Comparable<Font> {

    

    
    public static final int COURIER = 0;

    
    public static final int HELVETICA = 1;

    
    public static final int TIMES_ROMAN = 2;

    
    public static final int SYMBOL = 3;

    
    public static final int ZAPFDINGBATS = 4;

    

    
    public static final int NORMAL = 0;

    
    public static final int BOLD = 1;

    
    public static final int ITALIC = 2;

    
    public static final int UNDERLINE = 4;

    
    public static final int STRIKETHRU = 8;

    
    public static final int BOLDITALIC = BOLD | ITALIC;

    

    
    public static final int UNDEFINED = -1;

    
    public static final int DEFAULTSIZE = 12;

    

    
    private int family = UNDEFINED;

    
    private float size = UNDEFINED;

    
    private int style = UNDEFINED;

    
    private Color color = null;

    
    private BaseFont baseFont = null;

    

    
    public Font(Font other) {
        this.family = other.family;
        this.size = other.size;
        this.style = other.style;
        this.color = other.color;
        this.baseFont = other.baseFont;
    }

    

    public Font(int family, float size, int style, Color color) {
        this.family = family;
        this.size = size;
        this.style = style;
        this.color = color;
    }

    

    public Font(BaseFont bf, float size, int style, Color color) {
        this.baseFont = bf;
        this.size = size;
        this.style = style;
        this.color = color;
    }

    
    public Font(BaseFont bf, float size, int style) {
        this(bf, size, style, null);
    }

    
    public Font(BaseFont bf, float size) {
        this(bf, size, UNDEFINED, null);
    }

    
    public Font(BaseFont bf) {
        this(bf, UNDEFINED, UNDEFINED, null);
    }

    

    public Font(int family, float size, int style) {
        this(family, size, style, null);
    }

    

    public Font(int family, float size) {
        this(family, size, UNDEFINED, null);
    }

    

    public Font(int family) {
        this(family, UNDEFINED, UNDEFINED, null);
    }

    

    public Font() {
        this(UNDEFINED, UNDEFINED, UNDEFINED, null);
    }

    

    
    public int compareTo(Font font) {
        if (font == null) {
            return -1;
        }
            if (baseFont != null && !baseFont.equals(font.getBaseFont())) {
                return -2;
            }
            if (this.family != font.getFamily()) {
                return 1;
            }
            if (this.size != font.getSize()) {
                return 2;
            }
            if (this.style != font.getStyle()) {
                return 3;
            }
            if (this.color == null) {
                if (font.color == null) {
                    return 0;
                }
                return 4;
            }
            if (font.color == null) {
                return 4;
            }
            if (this.color.equals(font.getColor())) {
                return 0;
            }
            return 4;
    }

    

    
    public int getFamily() {
        return family;
    }

    
    public String getFamilyname() {
        String tmp = "unknown";
        switch (getFamily()) {
        case Font.COURIER:
            return FontFactory.COURIER;
        case Font.HELVETICA:
            return FontFactory.HELVETICA;
        case Font.TIMES_ROMAN:
            return FontFactory.TIMES_ROMAN;
        case Font.SYMBOL:
            return FontFactory.SYMBOL;
        case Font.ZAPFDINGBATS:
            return FontFactory.ZAPFDINGBATS;
        default:
            if (baseFont != null) {
                String[][] names = baseFont.getFamilyFontName();
                for (int i = 0; i < names.length; i++) {
                    if ("0".equals(names[i][2])) {
                        return names[i][3];
                    }
                    if ("1033".equals(names[i][2])) {
                        tmp = names[i][3];
                    }
                    if ("".equals(names[i][2])) {
                        tmp = names[i][3];
                    }
                }
            }
        }
        return tmp;
    }

    
    public void setFamily(String family) {
        this.family = getFamilyIndex(family);
    }

    
    public static int getFamilyIndex(String family) {
        if (family.equalsIgnoreCase(FontFactory.COURIER)) {
            return COURIER;
        }
        if (family.equalsIgnoreCase(FontFactory.HELVETICA)) {
            return HELVETICA;
        }
        if (family.equalsIgnoreCase(FontFactory.TIMES_ROMAN)) {
            return TIMES_ROMAN;
        }
        if (family.equalsIgnoreCase(FontFactory.SYMBOL)) {
            return SYMBOL;
        }
        if (family.equalsIgnoreCase(FontFactory.ZAPFDINGBATS)) {
            return ZAPFDINGBATS;
        }
        return UNDEFINED;
    }

    
    
    
    public float getSize() {
        return size;
    }

    
    public float getCalculatedSize() {
        float s = this.size;
        if (s == UNDEFINED) {
            s = DEFAULTSIZE;
        }
        return s;
    }

    
    public float getCalculatedLeading(float linespacing) {
        return linespacing * getCalculatedSize();
    }

    
    public void setSize(float size) {
        this.size = size;
    }

    
    
    
    public int getStyle() {
        return style;
    }

    
    public int getCalculatedStyle() {
        int style = this.style;
        if (style == UNDEFINED) {
            style = NORMAL;
        }
        if (baseFont != null)
            return style;
        if (family == SYMBOL || family == ZAPFDINGBATS)
            return style;
        else
            return style & (~BOLDITALIC);
    }

    
    public boolean isBold() {
        if (style == UNDEFINED) {
            return false;
        }
        return (style & BOLD) == BOLD;
    }

    
    public boolean isItalic() {
        if (style == UNDEFINED) {
            return false;
        }
        return (style & ITALIC) == ITALIC;
    }

    
    public boolean isUnderlined() {
        if (style == UNDEFINED) {
            return false;
        }
        return (style & UNDERLINE) == UNDERLINE;
    }

    
    public boolean isStrikethru() {
        if (style == UNDEFINED) {
            return false;
        }
        return (style & STRIKETHRU) == STRIKETHRU;
    }

    
    public void setStyle(int style) {
        if (this.style == UNDEFINED)
            this.style = NORMAL;
        this.style |= style;
    }

    
    public void setStyle(String style) {
        if (this.style == UNDEFINED)
            this.style = NORMAL;
        this.style |= getStyleValue(style);
    }

    
    public static int getStyleValue(String style) {
        int s = 0;
        if (style.indexOf(Markup.CSS_VALUE_NORMAL) != -1) {
            s |= NORMAL;
        }
        if (style.indexOf(Markup.CSS_VALUE_BOLD) != -1) {
            s |= BOLD;
        }
        if (style.indexOf(Markup.CSS_VALUE_ITALIC) != -1) {
            s |= ITALIC;
        }
        if (style.indexOf(Markup.CSS_VALUE_OBLIQUE) != -1) {
            s |= ITALIC;
        }
        if (style.indexOf(Markup.CSS_VALUE_UNDERLINE) != -1) {
            s |= UNDERLINE;
        }
        if (style.indexOf(Markup.CSS_VALUE_LINETHROUGH) != -1) {
            s |= STRIKETHRU;
        }
        return s;
    }

    
    
    
    public Color getColor() {
        return color;
    }

    

    public void setColor(Color color) {
        this.color = color;
    }

    
    public void setColor(int red, int green, int blue) {
        this.color = new Color(red, green, blue);
    }

    

    
    public BaseFont getBaseFont() {
        return baseFont;
    }

    
    public BaseFont getCalculatedBaseFont(boolean specialEncoding) {
        if (baseFont != null)
            return baseFont;
        int style = this.style;
        if (style == UNDEFINED) {
            style = NORMAL;
        }
        String fontName = BaseFont.HELVETICA;
        String encoding = BaseFont.WINANSI;
        BaseFont cfont = null;
        switch (family) {
        case COURIER:
            switch (style & BOLDITALIC) {
            case BOLD:
                fontName = BaseFont.COURIER_BOLD;
                break;
            case ITALIC:
                fontName = BaseFont.COURIER_OBLIQUE;
                break;
            case BOLDITALIC:
                fontName = BaseFont.COURIER_BOLDOBLIQUE;
                break;
            default:
                
                fontName = BaseFont.COURIER;
                break;
            }
            break;
        case TIMES_ROMAN:
            switch (style & BOLDITALIC) {
            case BOLD:
                fontName = BaseFont.TIMES_BOLD;
                break;
            case ITALIC:
                fontName = BaseFont.TIMES_ITALIC;
                break;
            case BOLDITALIC:
                fontName = BaseFont.TIMES_BOLDITALIC;
                break;
            default:
            case NORMAL:
                fontName = BaseFont.TIMES_ROMAN;
                break;
            }
            break;
        case SYMBOL:
            fontName = BaseFont.SYMBOL;
            if (specialEncoding)
                encoding = BaseFont.SYMBOL;
            break;
        case ZAPFDINGBATS:
            fontName = BaseFont.ZAPFDINGBATS;
            if (specialEncoding)
                encoding = BaseFont.ZAPFDINGBATS;
            break;
        default:
        case Font.HELVETICA:
            switch (style & BOLDITALIC) {
            case BOLD:
                fontName = BaseFont.HELVETICA_BOLD;
                break;
            case ITALIC:
                fontName = BaseFont.HELVETICA_OBLIQUE;
                break;
            case BOLDITALIC:
                fontName = BaseFont.HELVETICA_BOLDOBLIQUE;
                break;
            default:
            case NORMAL:
                fontName = BaseFont.HELVETICA;
                break;
            }
            break;
        }
        try {
            cfont = BaseFont.createFont(fontName, encoding, false);
        } catch (Exception ee) {
            throw new ExceptionConverter(ee);
        }
        return cfont;
    }
    
    
    

    
    public boolean isStandardFont() {
        return (family == UNDEFINED && size == UNDEFINED && style == UNDEFINED
                && color == null && baseFont == null);
    }

    
    public Font difference(Font font) {
        if (font == null) return this;
        
        float dSize = font.size;
        if (dSize == UNDEFINED) {
            dSize = this.size;
        }
        
        int dStyle = UNDEFINED;
        int style1 = this.style;
        int style2 = font.getStyle();
        if (style1 != UNDEFINED || style2 != UNDEFINED) {
            if (style1 == UNDEFINED)
                style1 = 0;
            if (style2 == UNDEFINED)
                style2 = 0;
            dStyle = style1 | style2;
        }
        
        Color dColor = font.color;
        if (dColor == null) {
            dColor = this.color;
        }
        
        if (font.baseFont != null) {
            return new Font(font.baseFont, dSize, dStyle, dColor);
        }
        if (font.getFamily() != UNDEFINED) {
            return new Font(font.family, dSize, dStyle, dColor);
        }
        if (this.baseFont != null) {
            if (dStyle == style1) {
                return new Font(this.baseFont, dSize, dStyle, dColor);
            } else {
                return FontFactory.getFont(this.getFamilyname(), dSize, dStyle,
                        dColor);
            }
        }
        return new Font(this.family, dSize, dStyle, dColor);
    }

    

    
    public int family() {
        return getFamily();
    }

    
    public float size() {
        return getSize();
    }
    
    
    public float leading(float linespacing) {
        return getCalculatedLeading(linespacing);
    }

    
    public int style() {
        return getStyle();
    }

    
    public Color color() {
        return getColor();
    }
}
