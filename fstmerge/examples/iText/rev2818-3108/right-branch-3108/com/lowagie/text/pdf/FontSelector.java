
package com.lowagie.text.pdf;

import java.util.ArrayList;

import com.lowagie.text.Chunk;
import com.lowagie.text.Font;
import com.lowagie.text.Phrase;


public class FontSelector {
    
    protected ArrayList<Font> fonts = new ArrayList<Font>();

        
    public void addFont(Font font) {
        if (font.getBaseFont() != null) {
            fonts.add(font);
            return;
        }
        BaseFont bf = font.getCalculatedBaseFont(true);
        Font f2 = new Font(bf, font.getSize(), font.getCalculatedStyle(), font.getColor());
        fonts.add(f2);
    }
    
        
    public Phrase process(String text) {
        int fsize = fonts.size();
        if (fsize == 0)
            throw new IndexOutOfBoundsException("No font is defined.");
        char cc[] = text.toCharArray();
        int len = cc.length;
        StringBuffer sb = new StringBuffer();
        Font font = null;
        int lastidx = -1;
        Phrase ret = new Phrase();
        for (int k = 0; k < len; ++k) {
            char c = cc[k];
            if (c == '\n' || c == '\r') {
                sb.append(c);
                continue;
            }
            for (int f = 0; f < fsize; ++f) {
                font = fonts.get(f);
                if (font.getBaseFont().charExists(c)) {
                    if (lastidx == f)
                        sb.append(c);
                    else {
                        if (sb.length() > 0 && lastidx != -1) {
                            Chunk ck = new Chunk(sb.toString(), fonts.get(lastidx));
                            ret.add(ck);
                            sb.setLength(0);
                        }
                        sb.append(c);
                        lastidx = f;
                    }
                    break;
                }
            }
        }
        if (sb.length() > 0) {
            Chunk ck = new Chunk(sb.toString(), fonts.get(lastidx == -1 ? 0 : lastidx));
            ret.add(ck);
        }
        return ret;
    }
}
