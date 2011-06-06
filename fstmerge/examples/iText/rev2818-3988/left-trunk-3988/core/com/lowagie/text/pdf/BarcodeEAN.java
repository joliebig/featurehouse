
package com.lowagie.text.pdf;

import java.awt.Canvas;
import java.awt.Color;
import java.awt.Image;
import java.awt.image.MemoryImageSource;
import java.util.Arrays;

import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Rectangle;


public class BarcodeEAN extends Barcode{
        
        
    private static final int GUARD_EMPTY[] = {};
        
    private static final int GUARD_UPCA[] = {0, 2, 4, 6, 28, 30, 52, 54, 56, 58};
        
    private static final int GUARD_EAN13[] = {0, 2, 28, 30, 56, 58};
        
    private static final int GUARD_EAN8[] = {0, 2, 20, 22, 40, 42};
        
    private static final int GUARD_UPCE[] = {0, 2, 28, 30, 32};
    
    private static final float TEXTPOS_EAN13[] = {6.5f, 13.5f, 20.5f, 27.5f, 34.5f, 41.5f, 53.5f, 60.5f, 67.5f, 74.5f, 81.5f, 88.5f};
    
    private static final float TEXTPOS_EAN8[] = {6.5f, 13.5f, 20.5f, 27.5f, 39.5f, 46.5f, 53.5f, 60.5f};
    
    private static final byte BARS[][] = 
    {
        {3, 2, 1, 1}, 
        {2, 2, 2, 1}, 
        {2, 1, 2, 2}, 
        {1, 4, 1, 1}, 
        {1, 1, 3, 2}, 
        {1, 2, 3, 1}, 
        {1, 1, 1, 4}, 
        {1, 3, 1, 2}, 
        {1, 2, 1, 3}, 
        {3, 1, 1, 2}  
    };
    
    
    private static final int TOTALBARS_EAN13 = 11 + 12 * 4;
    
    private static final int TOTALBARS_EAN8 = 11 + 8 * 4;
    
    private static final int TOTALBARS_UPCE = 9 + 6 * 4;
    
    private static final int TOTALBARS_SUPP2 = 13;
    
    private static final int TOTALBARS_SUPP5 = 31;
    
    private static final int ODD = 0;
    
    private static final int EVEN = 1;
    
    
    private static final byte PARITY13[][] =
    {
        {ODD, ODD,  ODD,  ODD,  ODD,  ODD},  
        {ODD, ODD,  EVEN, ODD,  EVEN, EVEN}, 
        {ODD, ODD,  EVEN, EVEN, ODD,  EVEN}, 
        {ODD, ODD,  EVEN, EVEN, EVEN, ODD},  
        {ODD, EVEN, ODD,  ODD,  EVEN, EVEN}, 
        {ODD, EVEN, EVEN, ODD,  ODD,  EVEN}, 
        {ODD, EVEN, EVEN, EVEN, ODD,  ODD},  
        {ODD, EVEN, ODD,  EVEN, ODD,  EVEN}, 
        {ODD, EVEN, ODD,  EVEN, EVEN, ODD},  
        {ODD, EVEN, EVEN, ODD,  EVEN, ODD}   
    };
    
    
    private static final byte PARITY2[][] =
    {
        {ODD,  ODD},   
        {ODD,  EVEN},  
        {EVEN, ODD},   
        {EVEN, EVEN}   
    };
    
    
    private static final byte PARITY5[][] =
    {
        {EVEN, EVEN, ODD,  ODD,  ODD},  
        {EVEN, ODD,  EVEN, ODD,  ODD},  
        {EVEN, ODD,  ODD,  EVEN, ODD},  
        {EVEN, ODD,  ODD,  ODD,  EVEN}, 
        {ODD,  EVEN, EVEN, ODD,  ODD},  
        {ODD,  ODD,  EVEN, EVEN, ODD},  
        {ODD,  ODD,  ODD,  EVEN, EVEN}, 
        {ODD,  EVEN, ODD,  EVEN, ODD},  
        {ODD,  EVEN, ODD,  ODD,  EVEN}, 
        {ODD,  ODD,  EVEN, ODD,  EVEN}  
    };
    
    
    private static final byte PARITYE[][] =
    {
        {EVEN, EVEN, EVEN, ODD,  ODD,  ODD},  
        {EVEN, EVEN, ODD,  EVEN, ODD,  ODD},  
        {EVEN, EVEN, ODD,  ODD,  EVEN, ODD},  
        {EVEN, EVEN, ODD,  ODD,  ODD,  EVEN}, 
        {EVEN, ODD,  EVEN, EVEN, ODD,  ODD},  
        {EVEN, ODD,  ODD,  EVEN, EVEN, ODD},  
        {EVEN, ODD,  ODD,  ODD,  EVEN, EVEN}, 
        {EVEN, ODD,  EVEN, ODD,  EVEN, ODD},  
        {EVEN, ODD,  EVEN, ODD,  ODD,  EVEN}, 
        {EVEN, ODD,  ODD,  EVEN, ODD,  EVEN}  
    };
    
    
    public BarcodeEAN() {
        try {
            x = 0.8f;
            font = BaseFont.createFont("Helvetica", "winansi", false);
            size = 8;
            baseline = size;
            barHeight = size * 3;
            guardBars = true;
            codeType = EAN13;
            code = "";
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
        
    public static int calculateEANParity(String code) {
        int mul = 3;
        int total = 0;
        for (int k = code.length() - 1; k >= 0; --k) {
            int n = code.charAt(k) - '0';
            total += mul * n;
            mul ^= 2;
        }
        return (10 - (total % 10)) % 10;
    }
    
        
    static public String convertUPCAtoUPCE(String text) {
        if (text.length() != 12 || !(text.startsWith("0") || text.startsWith("1")))
            return null;
        if (text.substring(3, 6).equals("000") || text.substring(3, 6).equals("100")
            || text.substring(3, 6).equals("200")) {
                if (text.substring(6, 8).equals("00"))
                    return text.substring(0, 1) + text.substring(1, 3) + text.substring(8, 11) + text.substring(3, 4) + text.substring(11);
        }
        else if (text.substring(4, 6).equals("00")) {
            if (text.substring(6, 9).equals("000"))
                return text.substring(0, 1) + text.substring(1, 4) + text.substring(9, 11) + "3" + text.substring(11);
        }
        else if (text.substring(5, 6).equals("0")) {
            if (text.substring(6, 10).equals("0000"))
                return text.substring(0, 1) + text.substring(1, 5) + text.substring(10, 11) + "4" + text.substring(11);
        }
        else if (text.charAt(10) >= '5') {
            if (text.substring(6, 10).equals("0000"))
                return text.substring(0, 1) + text.substring(1, 6) + text.substring(10, 11) + text.substring(11);
        }
        return null;
    }
    
        
    public static byte[] getBarsEAN13(String _code) {
        int code[] = new int[_code.length()];
        for (int k = 0; k < code.length; ++k)
            code[k] = _code.charAt(k) - '0';
        byte bars[] = new byte[TOTALBARS_EAN13];
        int pb = 0;
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        byte sequence[] = PARITY13[code[0]];
        for (int k = 0; k < sequence.length; ++k) {
            int c = code[k + 1];
            byte stripes[] = BARS[c];
            if (sequence[k] == ODD) {
                bars[pb++] = stripes[0];
                bars[pb++] = stripes[1];
                bars[pb++] = stripes[2];
                bars[pb++] = stripes[3];
            }
            else {
                bars[pb++] = stripes[3];
                bars[pb++] = stripes[2];
                bars[pb++] = stripes[1];
                bars[pb++] = stripes[0];
            }
        }
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        for (int k = 7; k < 13; ++k) {
            int c = code[k];
            byte stripes[] = BARS[c];
            bars[pb++] = stripes[0];
            bars[pb++] = stripes[1];
            bars[pb++] = stripes[2];
            bars[pb++] = stripes[3];
        }
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        return bars;
    }
    
        
    public static byte[] getBarsEAN8(String _code) {
        int code[] = new int[_code.length()];
        for (int k = 0; k < code.length; ++k)
            code[k] = _code.charAt(k) - '0';
        byte bars[] = new byte[TOTALBARS_EAN8];
        int pb = 0;
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        for (int k = 0; k < 4; ++k) {
            int c = code[k];
            byte stripes[] = BARS[c];
            bars[pb++] = stripes[0];
            bars[pb++] = stripes[1];
            bars[pb++] = stripes[2];
            bars[pb++] = stripes[3];
        }
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        for (int k = 4; k < 8; ++k) {
            int c = code[k];
            byte stripes[] = BARS[c];
            bars[pb++] = stripes[0];
            bars[pb++] = stripes[1];
            bars[pb++] = stripes[2];
            bars[pb++] = stripes[3];
        }
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        return bars;
    }
    
        
    public static byte[] getBarsUPCE(String _code) {
        int code[] = new int[_code.length()];
        for (int k = 0; k < code.length; ++k)
            code[k] = _code.charAt(k) - '0';
        byte bars[] = new byte[TOTALBARS_UPCE];
        boolean flip = (code[0] != 0);
        int pb = 0;
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        byte sequence[] = PARITYE[code[code.length - 1]];
        for (int k = 1; k < code.length - 1; ++k) {
            int c = code[k];
            byte stripes[] = BARS[c];
            if (sequence[k - 1] == (flip ? EVEN : ODD)) {
                bars[pb++] = stripes[0];
                bars[pb++] = stripes[1];
                bars[pb++] = stripes[2];
                bars[pb++] = stripes[3];
            }
            else {
                bars[pb++] = stripes[3];
                bars[pb++] = stripes[2];
                bars[pb++] = stripes[1];
                bars[pb++] = stripes[0];
            }
        }
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 1;
        return bars;
    }

        
    public static byte[] getBarsSupplemental2(String _code) {
        int code[] = new int[2];
        for (int k = 0; k < code.length; ++k)
            code[k] = _code.charAt(k) - '0';
        byte bars[] = new byte[TOTALBARS_SUPP2];
        int pb = 0;
        int parity = (code[0] * 10 + code[1]) % 4;
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 2;
        byte sequence[] = PARITY2[parity];
        for (int k = 0; k < sequence.length; ++k) {
            if (k == 1) {
                bars[pb++] = 1;
                bars[pb++] = 1;
            }
            int c = code[k];
            byte stripes[] = BARS[c];
            if (sequence[k] == ODD) {
                bars[pb++] = stripes[0];
                bars[pb++] = stripes[1];
                bars[pb++] = stripes[2];
                bars[pb++] = stripes[3];
            }
            else {
                bars[pb++] = stripes[3];
                bars[pb++] = stripes[2];
                bars[pb++] = stripes[1];
                bars[pb++] = stripes[0];
            }
        }
        return bars;
    }   

        
    public static byte[] getBarsSupplemental5(String _code) {
        int code[] = new int[5];
        for (int k = 0; k < code.length; ++k)
            code[k] = _code.charAt(k) - '0';
        byte bars[] = new byte[TOTALBARS_SUPP5];
        int pb = 0;
        int parity = (((code[0] + code[2] + code[4]) * 3) + ((code[1] + code[3]) * 9)) % 10;
        bars[pb++] = 1;
        bars[pb++] = 1;
        bars[pb++] = 2;
        byte sequence[] = PARITY5[parity];
        for (int k = 0; k < sequence.length; ++k) {
            if (k != 0) {
                bars[pb++] = 1;
                bars[pb++] = 1;
            }
            int c = code[k];
            byte stripes[] = BARS[c];
            if (sequence[k] == ODD) {
                bars[pb++] = stripes[0];
                bars[pb++] = stripes[1];
                bars[pb++] = stripes[2];
                bars[pb++] = stripes[3];
            }
            else {
                bars[pb++] = stripes[3];
                bars[pb++] = stripes[2];
                bars[pb++] = stripes[1];
                bars[pb++] = stripes[0];
            }
        }
        return bars;
    }   
    
        
    public Rectangle getBarcodeSize() {
        float width = 0;
        float height = barHeight;
        if (font != null) {
            if (baseline <= 0)
                height += -baseline + size;
            else
                height += baseline - font.getFontDescriptor(BaseFont.DESCENT, size);
        }
        switch (codeType) {
            case EAN13:
                width = x * (11 + 12 * 7);
                if (font != null) {
                    width += font.getWidthPoint(code.charAt(0), size);
                }
                break;
            case EAN8:
                width = x * (11 + 8 * 7);
                break;
            case UPCA:
                width = x * (11 + 12 * 7);
                if (font != null) {
                    width += font.getWidthPoint(code.charAt(0), size) + font.getWidthPoint(code.charAt(11), size);
                }
                break;
            case UPCE:
                width = x * (9 + 6 * 7);
                if (font != null) {
                    width += font.getWidthPoint(code.charAt(0), size) + font.getWidthPoint(code.charAt(7), size);
                }
                break;
            case SUPP2:
                width = x * (6 + 2 * 7);
                break;
            case SUPP5:
                width = x * (4 + 5 * 7 + 4 * 2);
                break;
            default:
                throw new RuntimeException("Invalid code type.");
        }
        return new Rectangle(width, height);
    }
    
        
    public Rectangle placeBarcode(PdfContentByte cb, Color barColor, Color textColor) {
        Rectangle rect = getBarcodeSize();
        float barStartX = 0;
        float barStartY = 0;
        float textStartY = 0;
        if (font != null) {
            if (baseline <= 0)
                textStartY = barHeight - baseline;
            else {
                textStartY = -font.getFontDescriptor(BaseFont.DESCENT, size);
                barStartY = textStartY + baseline;
            }
        }
        switch (codeType) {
            case EAN13:
            case UPCA:
            case UPCE:
                if (font != null)
                    barStartX += font.getWidthPoint(code.charAt(0), size);
                break;
        }
        byte bars[] = null;
        int guard[] = GUARD_EMPTY;
        switch (codeType) {
            case EAN13:
                bars = getBarsEAN13(code);
                guard = GUARD_EAN13;
                break;
            case EAN8:
                bars = getBarsEAN8(code);
                guard = GUARD_EAN8;
                break;
            case UPCA:
                bars = getBarsEAN13("0" + code);
                guard = GUARD_UPCA;
                break;
            case UPCE:
                bars = getBarsUPCE(code);
                guard = GUARD_UPCE;
                break;
            case SUPP2:
                bars = getBarsSupplemental2(code);
                break;
            case SUPP5:
                bars = getBarsSupplemental5(code);
                break;
        }
        float keepBarX = barStartX;
        boolean print = true;
        float gd = 0;
        if (font != null && baseline > 0 && guardBars) {
            gd = baseline / 2;
        }
        if (barColor != null)
            cb.setColorFill(barColor);
        for (int k = 0; k < bars.length; ++k) {
            float w = bars[k] * x;
            if (print) {
                if (Arrays.binarySearch(guard, k) >= 0)
                    cb.rectangle(barStartX, barStartY - gd, w - inkSpreading, barHeight + gd);
                else
                    cb.rectangle(barStartX, barStartY, w - inkSpreading, barHeight);
            }
            print = !print;
            barStartX += w;
        }
        cb.fill();
        if (font != null) {
            if (textColor != null)
                cb.setColorFill(textColor);
            cb.beginText();
            cb.setFontAndSize(font, size);
            switch (codeType) {
                case EAN13:
                    cb.setTextMatrix(0, textStartY);
                    cb.showText(code.substring(0, 1));
                    for (int k = 1; k < 13; ++k) {
                        String c = code.substring(k, k + 1);
                        float len = font.getWidthPoint(c, size);
                        float pX = keepBarX + TEXTPOS_EAN13[k - 1] * x - len / 2;
                        cb.setTextMatrix(pX, textStartY);
                        cb.showText(c);
                    }
                    break;
                case EAN8:
                    for (int k = 0; k < 8; ++k) {
                        String c = code.substring(k, k + 1);
                        float len = font.getWidthPoint(c, size);
                        float pX = TEXTPOS_EAN8[k] * x - len / 2;
                        cb.setTextMatrix(pX, textStartY);
                        cb.showText(c);
                    }
                    break;
                case UPCA:
                    cb.setTextMatrix(0, textStartY);
                    cb.showText(code.substring(0, 1));
                    for (int k = 1; k < 11; ++k) {
                        String c = code.substring(k, k + 1);
                        float len = font.getWidthPoint(c, size);
                        float pX = keepBarX + TEXTPOS_EAN13[k] * x - len / 2;
                        cb.setTextMatrix(pX, textStartY);
                        cb.showText(c);
                    }
                    cb.setTextMatrix(keepBarX + x * (11 + 12 * 7), textStartY);
                    cb.showText(code.substring(11, 12));
                    break;
                case UPCE:
                    cb.setTextMatrix(0, textStartY);
                    cb.showText(code.substring(0, 1));
                    for (int k = 1; k < 7; ++k) {
                        String c = code.substring(k, k + 1);
                        float len = font.getWidthPoint(c, size);
                        float pX = keepBarX + TEXTPOS_EAN13[k - 1] * x - len / 2;
                        cb.setTextMatrix(pX, textStartY);
                        cb.showText(c);
                    }
                    cb.setTextMatrix(keepBarX + x * (9 + 6 * 7), textStartY);
                    cb.showText(code.substring(7, 8));
                    break;
                case SUPP2:
                case SUPP5:
                    for (int k = 0; k < code.length(); ++k) {
                        String c = code.substring(k, k + 1);
                        float len = font.getWidthPoint(c, size);
                        float pX = (7.5f + (9 * k)) * x - len / 2;
                        cb.setTextMatrix(pX, textStartY);
                        cb.showText(c);
                    }
                    break;
            }
            cb.endText();
        }
        return rect;
    }
    
        
    public java.awt.Image createAwtImage(Color foreground, Color background) {
        int f = foreground.getRGB();
        int g = background.getRGB();
        Canvas canvas = new Canvas();

        int width = 0;
        byte bars[] = null;
        switch (codeType) {
            case EAN13:
                bars = getBarsEAN13(code);
                width = 11 + 12 * 7;
                break;
            case EAN8:
                bars = getBarsEAN8(code);
                width = 11 + 8 * 7;
                break;
            case UPCA:
                bars = getBarsEAN13("0" + code);
                width = 11 + 12 * 7;
                break;
            case UPCE:
                bars = getBarsUPCE(code);
                width = 9 + 6 * 7;
                break;
            case SUPP2:
                bars = getBarsSupplemental2(code);
                width = 6 + 2 * 7;
                break;
            case SUPP5:
                bars = getBarsSupplemental5(code);
                width = 4 + 5 * 7 + 4 * 2;
                break;
            default:
                throw new RuntimeException("Invalid code type.");
        }

        boolean print = true;
        int ptr = 0;
        int height = (int)barHeight;
        int pix[] = new int[width * height];
        for (int k = 0; k < bars.length; ++k) {
            int w = bars[k];
            int c = g;
            if (print)
                c = f;
            print = !print;
            for (int j = 0; j < w; ++j)
                pix[ptr++] = c;
        }
        for (int k = width; k < pix.length; k += width) {
            System.arraycopy(pix, 0, pix, k, width); 
        }
        Image img = canvas.createImage(new MemoryImageSource(width, height, pix, 0, width));
        
        return img;
    }    
}
