

package com.lowagie.text.pdf;

import com.lowagie.text.SplitCharacter;


public class DefaultSplitCharacter implements SplitCharacter {
    
    
    public static final SplitCharacter DEFAULT = new DefaultSplitCharacter();
    
    
    public boolean isSplitCharacter(int start, int current, int end, char[] cc, PdfChunk[] ck) {
        char c = getCurrentCharacter(current, cc, ck);
        if (c <= ' ' || c == '-' || c == '\u') {
            return true;
        }
        if (c < 0x2002)
            return false;
        return ((c >= 0x2002 && c <= 0x200b)
        || (c >= 0x2e80 && c < 0xd7a0)
        || (c >= 0xf900 && c < 0xfb00)
        || (c >= 0xfe30 && c < 0xfe50)
        || (c >= 0xff61 && c < 0xffa0));
    }

    
    protected char getCurrentCharacter(int current, char[] cc, PdfChunk[] ck) {
        if (ck == null) {
            return (char)cc[current];
        }
        return (char)ck[Math.min(current, ck.length - 1)].getUnicodeEquivalent(cc[current]);
    }
}
