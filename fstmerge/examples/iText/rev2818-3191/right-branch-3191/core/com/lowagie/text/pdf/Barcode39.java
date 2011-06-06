
package com.lowagie.text.pdf;

import java.awt.Canvas;
import java.awt.Color;
import java.awt.Image;
import java.awt.image.MemoryImageSource;

import com.lowagie.text.Element;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Rectangle;


public class Barcode39 extends Barcode{

        
    private static final byte BARS[][] = 
    {
        {0,0,0,1,1,0,1,0,0},
        {1,0,0,1,0,0,0,0,1},
        {0,0,1,1,0,0,0,0,1},
        {1,0,1,1,0,0,0,0,0},
        {0,0,0,1,1,0,0,0,1},
        {1,0,0,1,1,0,0,0,0},
        {0,0,1,1,1,0,0,0,0},
        {0,0,0,1,0,0,1,0,1},
        {1,0,0,1,0,0,1,0,0},
        {0,0,1,1,0,0,1,0,0},
        {1,0,0,0,0,1,0,0,1},
        {0,0,1,0,0,1,0,0,1},
        {1,0,1,0,0,1,0,0,0},
        {0,0,0,0,1,1,0,0,1},
        {1,0,0,0,1,1,0,0,0},
        {0,0,1,0,1,1,0,0,0},
        {0,0,0,0,0,1,1,0,1},
        {1,0,0,0,0,1,1,0,0},
        {0,0,1,0,0,1,1,0,0},
        {0,0,0,0,1,1,1,0,0},
        {1,0,0,0,0,0,0,1,1},
        {0,0,1,0,0,0,0,1,1},
        {1,0,1,0,0,0,0,1,0},
        {0,0,0,0,1,0,0,1,1},
        {1,0,0,0,1,0,0,1,0},
        {0,0,1,0,1,0,0,1,0},
        {0,0,0,0,0,0,1,1,1},
        {1,0,0,0,0,0,1,1,0},
        {0,0,1,0,0,0,1,1,0},
        {0,0,0,0,1,0,1,1,0},
        {1,1,0,0,0,0,0,0,1},
        {0,1,1,0,0,0,0,0,1},
        {1,1,1,0,0,0,0,0,0},
        {0,1,0,0,1,0,0,0,1},
        {1,1,0,0,1,0,0,0,0},
        {0,1,1,0,1,0,0,0,0},
        {0,1,0,0,0,0,1,0,1},
        {1,1,0,0,0,0,1,0,0},
        {0,1,1,0,0,0,1,0,0},
        {0,1,0,1,0,1,0,0,0},
        {0,1,0,1,0,0,0,1,0},
        {0,1,0,0,0,1,0,1,0},
        {0,0,0,1,0,1,0,1,0},
        {0,1,0,0,1,0,1,0,0}
    };
 
        
    private static final String CHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%*";
    
        
    private static final String EXTENDED = "%U" +
        "$A$B$C$D$E$F$G$H$I$J$K$L$M$N$O$P$Q$R$S$T$U$V$W$X$Y$Z" +
        "%A%B%C%D%E  /A/B/C/D/E/F/G/H/I/J/K/L - ./O" +
        " 0 1 2 3 4 5 6 7 8 9/Z%F%G%H%I%J%V" +
        " A B C D E F G H I J K L M N O P Q R S T U V W X Y Z" +
        "%K%L%M%N%O%W" +
        "+A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z" +
        "%P%Q%R%S%T";
        
        
    public Barcode39() {
        try {
            x = 0.8f;
            n = 2;
            font = BaseFont.createFont("Helvetica", "winansi", false);
            size = 8;
            baseline = size;
            barHeight = size * 3;
            textAlignment = Element.ALIGN_CENTER;
            generateChecksum = false;
            checksumText = false;
            startStopText = true;
            extended = false;
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
        
    public static byte[] getBarsCode39(String text) {
        text = "*" + text + "*";
        byte bars[] = new byte[text.length() * 10 - 1];
        for (int k = 0; k < text.length(); ++k) {
            int idx = CHARS.indexOf(text.charAt(k));
            if (idx < 0)
                throw new IllegalArgumentException("The character '" + text.charAt(k) + "' is illegal in code 39.");
            System.arraycopy(BARS[idx], 0, bars, k * 10, 9);
        }
        return bars;
    }
    
        
    public static String getCode39Ex(String text) {
        String out = "";
        for (int k = 0; k < text.length(); ++k) {
            char c = text.charAt(k);
            if (c > 127)
                throw new IllegalArgumentException("The character '" + c + "' is illegal in code 39 extended.");
            char c1 = EXTENDED.charAt(c * 2);
            char c2 = EXTENDED.charAt(c * 2 + 1);
            if (c1 != ' ')
                out += c1;
            out += c2;
        }
        return out;
    }
    
        
    static char getChecksum(String text) {
        int chk = 0;
        for (int k = 0; k < text.length(); ++k) {
            int idx = CHARS.indexOf(text.charAt(k));
            if (idx < 0)
                throw new IllegalArgumentException("The character '" + text.charAt(k) + "' is illegal in code 39.");
            chk += idx;
        }
        return CHARS.charAt(chk % 43);
    }
    
        
    public Rectangle getBarcodeSize() {
        float fontX = 0;
        float fontY = 0;
        String fCode = code;
        if (extended)
            fCode = getCode39Ex(code);
        if (font != null) {
            if (baseline > 0)
                fontY = baseline - font.getFontDescriptor(BaseFont.DESCENT, size);
            else
                fontY = -baseline + size;
            String fullCode = code;
            if (generateChecksum && checksumText)
                fullCode += getChecksum(fCode);
            if (startStopText)
                fullCode = "*" + fullCode + "*";
            fontX = font.getWidthPoint(altText != null ? altText : fullCode, size);
        }
        int len = fCode.length() + 2;
        if (generateChecksum)
            ++len;
        float fullWidth = len * (6 * x + 3 * x * n) + (len - 1) * x;
        fullWidth = Math.max(fullWidth, fontX);
        float fullHeight = barHeight + fontY;
        return new Rectangle(fullWidth, fullHeight);
    }
    
        
    public Rectangle placeBarcode(PdfContentByte cb, Color barColor, Color textColor) {
        String fullCode = code;
        float fontX = 0;
        String bCode = code;
        if (extended)
            bCode = getCode39Ex(code);
        if (font != null) {
            if (generateChecksum && checksumText)
                fullCode += getChecksum(bCode);
            if (startStopText)
                fullCode = "*" + fullCode + "*";
            fontX = font.getWidthPoint(fullCode = altText != null ? altText : fullCode, size);
        }
        if (generateChecksum)
            bCode += getChecksum(bCode);
        int len = bCode.length() + 2;
        float fullWidth = len * (6 * x + 3 * x * n) + (len - 1) * x;
        float barStartX = 0;
        float textStartX = 0;
        switch (textAlignment) {
            case Element.ALIGN_LEFT:
                break;
            case Element.ALIGN_RIGHT:
                if (fontX > fullWidth)
                    barStartX = fontX - fullWidth;
                else
                    textStartX = fullWidth - fontX;
                break;
            default:
                if (fontX > fullWidth)
                    barStartX = (fontX - fullWidth) / 2;
                else
                    textStartX = (fullWidth - fontX) / 2;
                break;
        }
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
        byte bars[] = getBarsCode39(bCode);
        boolean print = true;
        if (barColor != null)
            cb.setColorFill(barColor);
        for (int k = 0; k < bars.length; ++k) {
            float w = (bars[k] == 0 ? x : x * n);
            if (print)
                cb.rectangle(barStartX, barStartY, w - inkSpreading, barHeight);
            print = !print;
            barStartX += w;
        }
        cb.fill();
        if (font != null) {
            if (textColor != null)
                cb.setColorFill(textColor);
            cb.beginText();
            cb.setFontAndSize(font, size);
            cb.setTextMatrix(textStartX, textStartY);
            cb.showText(fullCode);
            cb.endText();
        }
        return getBarcodeSize();
    }
    
        
    public java.awt.Image createAwtImage(Color foreground, Color background) {
        int f = foreground.getRGB();
        int g = background.getRGB();
        Canvas canvas = new Canvas();

        String bCode = code;
        if (extended)
            bCode = getCode39Ex(code);
        if (generateChecksum)
            bCode += getChecksum(bCode);
        int len = bCode.length() + 2;
        int nn = (int)n;
        int fullWidth = len * (6 + 3 * nn) + (len - 1);
        byte bars[] = getBarsCode39(bCode);
        boolean print = true;
        int ptr = 0;
        int height = (int)barHeight;
        int pix[] = new int[fullWidth * height];
        for (int k = 0; k < bars.length; ++k) {
            int w = (bars[k] == 0 ? 1 : nn);
            int c = g;
            if (print)
                c = f;
            print = !print;
            for (int j = 0; j < w; ++j)
                pix[ptr++] = c;
        }
        for (int k = fullWidth; k < pix.length; k += fullWidth) {
            System.arraycopy(pix, 0, pix, k, fullWidth); 
        }
        Image img = canvas.createImage(new MemoryImageSource(fullWidth, height, pix, 0, fullWidth));
        
        return img;
    }    
}
