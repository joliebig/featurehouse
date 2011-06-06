
package com.lowagie.text.pdf;
import java.awt.Canvas;
import java.awt.Color;
import java.awt.Image;
import java.awt.image.MemoryImageSource;

import com.lowagie.text.Rectangle;


public class BarcodePostnet extends Barcode{

        
    private static final byte BARS[][] = 
    {
        {1,1,0,0,0},
        {0,0,0,1,1},
        {0,0,1,0,1},
        {0,0,1,1,0},
        {0,1,0,0,1},
        {0,1,0,1,0},
        {0,1,1,0,0},
        {1,0,0,0,1},
        {1,0,0,1,0},
        {1,0,1,0,0}
    };
    
    
    public BarcodePostnet() {
        n = 72f / 22f; 
        x = 0.02f * 72f; 
        barHeight = 0.125f * 72f; 
        size = 0.05f * 72f; 
        codeType = POSTNET; 
    }
    
        
    public static byte[] getBarsPostnet(String text) {
        int total = 0;
        for (int k = text.length() - 1; k >= 0; --k) {
            int n = text.charAt(k) - '0';
            total += n;
        }
        text += (char)(((10 - (total % 10)) % 10) + '0');
        byte bars[] = new byte[text.length() * 5 + 2];
        bars[0] = 1;
        bars[bars.length - 1] = 1;
        for (int k = 0; k < text.length(); ++k) {
            int c = text.charAt(k) - '0';
            System.arraycopy(BARS[c], 0, bars, k * 5 + 1, 5);
        }
        return bars;
    }

    
    public Rectangle getBarcodeSize() {
        float width = ((code.length() + 1) * 5 + 1) * n + x;
        return new Rectangle(width, barHeight);
    }
    
    
    public Rectangle placeBarcode(PdfContentByte cb, Color barColor, Color textColor) {
        if (barColor != null)
            cb.setColorFill(barColor);
        byte bars[] = getBarsPostnet(code);
        byte flip = 1;
        if (codeType == PLANET) {
            flip = 0;
            bars[0] = 0;
            bars[bars.length - 1] = 0;
        }
        float startX = 0;
        for (int k = 0; k < bars.length; ++k) {
            cb.rectangle(startX, 0, x - inkSpreading, bars[k] == flip ? barHeight : size);
            startX += n;
        }
        cb.fill();
        return getBarcodeSize();
    }
    
    
    public java.awt.Image createAwtImage(Color foreground, Color background) {
        int f = foreground.getRGB();
        int g = background.getRGB();
        Canvas canvas = new Canvas();
        int barWidth = (int)x;
        if (barWidth <= 0)
            barWidth = 1;
        int barDistance = (int)n;
        if (barDistance <= barWidth)
            barDistance = barWidth + 1;
        int barShort = (int)size;
        if (barShort <= 0)
            barShort = 1;
        int barTall = (int)barHeight;
        if (barTall <= barShort)
            barTall = barShort + 1;
        int width = ((code.length() + 1) * 5 + 1) * barDistance + barWidth;
        int pix[] = new int[width * barTall];
        byte bars[] = getBarsPostnet(code);
        byte flip = 1;
        if (codeType == PLANET) {
            flip = 0;
            bars[0] = 0;
            bars[bars.length - 1] = 0;
        }
        int idx = 0;
        for (int k = 0; k < bars.length; ++k) {
            boolean dot = (bars[k] == flip);
            for (int j = 0; j < barDistance; ++j) {
                pix[idx + j] = ((dot && j < barWidth) ? f : g);
            }
            idx += barDistance;
        }
        int limit = width * (barTall - barShort);
        for (int k = width; k < limit; k += width)
            System.arraycopy(pix, 0, pix, k, width);
        idx = limit;
        for (int k = 0; k < bars.length; ++k) {
            for (int j = 0; j < barDistance; ++j) {
                pix[idx + j] = ((j < barWidth) ? f : g);
            }
            idx += barDistance;
        }
        for (int k = limit + width; k < pix.length; k += width)
            System.arraycopy(pix, limit, pix, k, width);
        Image img = canvas.createImage(new MemoryImageSource(width, barTall, pix, 0, width));
        
        return img;
    }
}