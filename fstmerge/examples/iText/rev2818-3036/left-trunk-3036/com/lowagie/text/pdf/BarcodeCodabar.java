
package com.lowagie.text.pdf;

import java.awt.Canvas;
import java.awt.Color;
import java.awt.Image;
import java.awt.image.MemoryImageSource;

import com.lowagie.text.Element;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Rectangle;


public class BarcodeCodabar extends Barcode{

        
    private static final byte BARS[][] = 
    {
        {0,0,0,0,0,1,1}, 
        {0,0,0,0,1,1,0}, 
        {0,0,0,1,0,0,1}, 
        {1,1,0,0,0,0,0}, 
        {0,0,1,0,0,1,0}, 
        {1,0,0,0,0,1,0}, 
        {0,1,0,0,0,0,1}, 
        {0,1,0,0,1,0,0}, 
        {0,1,1,0,0,0,0}, 
        {1,0,0,1,0,0,0}, 
        {0,0,0,1,1,0,0}, 
        {0,0,1,1,0,0,0}, 
        {1,0,0,0,1,0,1}, 
        {1,0,1,0,0,0,1}, 
        {1,0,1,0,1,0,0}, 
        {0,0,1,0,1,0,1}, 
        {0,0,1,1,0,1,0}, 
        {0,1,0,1,0,0,1}, 
        {0,0,0,1,0,1,1}, 
        {0,0,0,1,1,1,0}  
    };
 
        
    private static final String CHARS = "0123456789-$:/.+ABCD";
    
    private static final int START_STOP_IDX = 16;    
        
    public BarcodeCodabar() {
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
            startStopText = false;
            codeType = CODABAR;
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
        
    public static byte[] getBarsCodabar(String text) {
        text = text.toUpperCase();
        int len = text.length();
        if (len < 2)
            throw new IllegalArgumentException("Codabar must have at least a start and stop character.");
        if (CHARS.indexOf(text.charAt(0)) < START_STOP_IDX || CHARS.indexOf(text.charAt(len - 1)) < START_STOP_IDX)
            throw new IllegalArgumentException("Codabar must have one of 'ABCD' as start/stop character.");
        byte bars[] = new byte[text.length() * 8 - 1];
        for (int k = 0; k < len; ++k) {
            int idx = CHARS.indexOf(text.charAt(k));
            if (idx >= START_STOP_IDX && k > 0 && k < len - 1)
                throw new IllegalArgumentException("In codabar, start/stop characters are only allowed at the extremes.");
            if (idx < 0)
                throw new IllegalArgumentException("The character '" + text.charAt(k) + "' is illegal in codabar.");
            System.arraycopy(BARS[idx], 0, bars, k * 8, 7);
        }
        return bars;
    }
    
    public static String calculateChecksum(String code) {
        if (code.length() < 2)
            return code;
        String text = code.toUpperCase();
        int sum = 0;
        int len = text.length();
        for (int k = 0; k < len; ++k)
            sum += CHARS.indexOf(text.charAt(k));
        sum = (sum + 15) / 16 * 16 - sum;
        return code.substring(0, len - 1) + CHARS.charAt(sum) + code.substring(len - 1);
    }
    
        
    public Rectangle getBarcodeSize() {
        float fontX = 0;
        float fontY = 0;
        String text = code;
        if (generateChecksum && checksumText)
            text = calculateChecksum(code);
        if (!startStopText)
            text = text.substring(1, text.length() - 1);
        if (font != null) {
            if (baseline > 0)
                fontY = baseline - font.getFontDescriptor(BaseFont.DESCENT, size);
            else
                fontY = -baseline + size;
            fontX = font.getWidthPoint(altText != null ? altText : text, size);
        }
        text = code;
        if (generateChecksum)
            text = calculateChecksum(code);
        byte bars[] = getBarsCodabar(text);
        int wide = 0;
        for (int k = 0; k < bars.length; ++k) {
            wide += (int)bars[k];
        }
        int narrow = bars.length - wide;
        float fullWidth = x * (narrow + wide * n);
        fullWidth = Math.max(fullWidth, fontX);
        float fullHeight = barHeight + fontY;
        return new Rectangle(fullWidth, fullHeight);
    }
    
        
    public Rectangle placeBarcode(PdfContentByte cb, Color barColor, Color textColor) {
        String fullCode = code;
        if (generateChecksum && checksumText)
            fullCode = calculateChecksum(code);
        if (!startStopText)
            fullCode = fullCode.substring(1, fullCode.length() - 1);
        float fontX = 0;
        if (font != null) {
            fontX = font.getWidthPoint(fullCode = altText != null ? altText : fullCode, size);
        }
        byte bars[] = getBarsCodabar(generateChecksum ? calculateChecksum(code) : code);
        int wide = 0;
        for (int k = 0; k < bars.length; ++k) {
            wide += (int)bars[k];
        }
        int narrow = bars.length - wide;
        float fullWidth = x * (narrow + wide * n);
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

        String fullCode = code;
        if (generateChecksum && checksumText)
            fullCode = calculateChecksum(code);
        if (!startStopText)
            fullCode = fullCode.substring(1, fullCode.length() - 1);
        byte bars[] = getBarsCodabar(generateChecksum ? calculateChecksum(code) : code);
        int wide = 0;
        for (int k = 0; k < bars.length; ++k) {
            wide += (int)bars[k];
        }
        int narrow = bars.length - wide;
        int fullWidth = narrow + wide * (int)n;
        boolean print = true;
        int ptr = 0;
        int height = (int)barHeight;
        int pix[] = new int[fullWidth * height];
        for (int k = 0; k < bars.length; ++k) {
            int w = (bars[k] == 0 ? 1 : (int)n);
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
