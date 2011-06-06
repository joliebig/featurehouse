

package com.lowagie.text.pdf;

import java.io.UnsupportedEncodingException;
import java.util.HashMap;

import com.lowagie.text.ExceptionConverter;

class FontDetails {
    
        
    PdfIndirectReference indirectReference;
        
    PdfName fontName;
        
    BaseFont baseFont;
        
    TrueTypeFontUnicode ttu;

    CJKFont cjkFont;
        
    byte shortTag[];
        
    HashMap longTag;
    
    IntHashtable cjkTag;
        
    int fontType;
        
    boolean symbolic;
    
    protected boolean subset = true;
    
    FontDetails(PdfName fontName, PdfIndirectReference indirectReference, BaseFont baseFont) {
        this.fontName = fontName;
        this.indirectReference = indirectReference;
        this.baseFont = baseFont;
        fontType = baseFont.getFontType();
        switch (fontType) {
            case BaseFont.FONT_TYPE_T1:
            case BaseFont.FONT_TYPE_TT:
                shortTag = new byte[256];
                break;
            case BaseFont.FONT_TYPE_CJK:
                cjkTag = new IntHashtable();
                cjkFont = (CJKFont)baseFont;
                break;
            case BaseFont.FONT_TYPE_TTUNI:
                longTag = new HashMap();
                ttu = (TrueTypeFontUnicode)baseFont;
                symbolic = baseFont.isFontSpecific();
                break;
        }
    }
    
        
    PdfIndirectReference getIndirectReference() {
        return indirectReference;
    }
    
        
    PdfName getFontName() {
        return fontName;
    }
    
        
    BaseFont getBaseFont() {
        return baseFont;
    }
    
        
    byte[] convertToBytes(String text) {
        byte b[] = null;
        switch (fontType) {
            case BaseFont.FONT_TYPE_T3:
                return baseFont.convertToBytes(text);
            case BaseFont.FONT_TYPE_T1:
            case BaseFont.FONT_TYPE_TT: {
                b = baseFont.convertToBytes(text);
                int len = b.length;
                for (int k = 0; k < len; ++k)
                    shortTag[((int)b[k]) & 0xff] = 1;
                break;
            }
            case BaseFont.FONT_TYPE_CJK: {
                int len = text.length();
                for (int k = 0; k < len; ++k)
                    cjkTag.put(cjkFont.getCidCode(text.charAt(k)), 0);
                b = baseFont.convertToBytes(text);
                break;
            }
            case BaseFont.FONT_TYPE_DOCUMENT: {
                b = baseFont.convertToBytes(text);
                break;
            }
            case BaseFont.FONT_TYPE_TTUNI: {
                try {
                    int len = text.length();
                    int metrics[] = null;
                    char glyph[] = new char[len];
                    int i = 0;
                    if (symbolic) {
                        b = PdfEncodings.convertToBytes(text, "symboltt");
                        len = b.length;
                        for (int k = 0; k < len; ++k) {
                            metrics = ttu.getMetricsTT(b[k] & 0xff);
                            if (metrics == null)
                                continue;
                            longTag.put(new Integer(metrics[0]), new int[]{metrics[0], metrics[1], ttu.getUnicodeDifferences(b[k] & 0xff)});
                            glyph[i++] = (char)metrics[0];
                        }
                    }
                    else {
                        for (int k = 0; k < len; ++k) {
                            char c = text.charAt(k);
                            metrics = ttu.getMetricsTT(c);
                            if (metrics == null)
                                continue;
                            int m0 = metrics[0];
                            Integer gl = new Integer(m0);
                            if (!longTag.containsKey(gl))
                                longTag.put(gl, new int[]{m0, metrics[1], c});
                            glyph[i++] = (char)m0;
                        }
                    }
                    String s = new String(glyph, 0, i);
                    b = s.getBytes(CJKFont.CJK_ENCODING);
                }
                catch (UnsupportedEncodingException e) {
                    throw new ExceptionConverter(e);
                }
                break;
            }
        }
        return b;
    }
    
        
    void writeFont(PdfWriter writer) {
        try {
            switch (fontType) {
                case BaseFont.FONT_TYPE_T3:
                    baseFont.writeFont(writer, indirectReference, null);
                    break;
                case BaseFont.FONT_TYPE_T1:
                case BaseFont.FONT_TYPE_TT: {
                    int firstChar;
                    int lastChar;
                    for (firstChar = 0; firstChar < 256; ++firstChar) {
                        if (shortTag[firstChar] != 0)
                            break;
                    }
                    for (lastChar = 255; lastChar >= firstChar; --lastChar) {
                        if (shortTag[lastChar] != 0)
                            break;
                    }
                    if (firstChar > 255) {
                        firstChar = 255;
                        lastChar = 255;
                    }
                    baseFont.writeFont(writer, indirectReference, new Object[]{new Integer(firstChar), new Integer(lastChar), shortTag, new Boolean(subset)});
                    break;
                }
                case BaseFont.FONT_TYPE_CJK:
                    baseFont.writeFont(writer, indirectReference, new Object[]{cjkTag});
                    break;
                case BaseFont.FONT_TYPE_TTUNI:
                    baseFont.writeFont(writer, indirectReference, new Object[]{longTag, new Boolean(subset)});
                    break;
            }
        }
        catch(Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
    
    public boolean isSubset() {
        return subset;
    }
    
    
    public void setSubset(boolean subset) {
        this.subset = subset;
    }
}
