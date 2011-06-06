

package com.lowagie.text.pdf;

import com.lowagie.text.pdf.hyphenation.Hyphenation;
import com.lowagie.text.pdf.hyphenation.Hyphenator;


public class HyphenationAuto implements HyphenationEvent {

        
    protected Hyphenator hyphenator;
        
    protected String post;
    
        
    public HyphenationAuto(String lang, String country, int leftMin, int rightMin) {
        hyphenator = new Hyphenator(lang, country, leftMin, rightMin);
    }

        
    public String getHyphenSymbol() {
        return "-";
    }
    
        
    public String getHyphenatedWordPre(String word, BaseFont font, float fontSize, float remainingWidth) {
        post = word;
        String hyphen = getHyphenSymbol();
        float hyphenWidth = font.getWidthPoint(hyphen, fontSize);
        if (hyphenWidth > remainingWidth)
            return "";
        Hyphenation hyphenation = hyphenator.hyphenate(word);
        if (hyphenation == null) {
            return "";
        }
        int len = hyphenation.length();
        int k;
        for (k = 0; k < len; ++k) {
            if (font.getWidthPoint(hyphenation.getPreHyphenText(k), fontSize) + hyphenWidth > remainingWidth)
                break;
        }
        --k;
        if (k < 0)
            return "";
        post = hyphenation.getPostHyphenText(k);
        return hyphenation.getPreHyphenText(k) + hyphen;
    }
    
        
    public String getHyphenatedWordPost() {
        return post;
    }
    
}
