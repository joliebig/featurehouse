

package com.lowagie.text.pdf.hyphenation;


public class Hyphenation {
    
    private int[] hyphenPoints;
    private String word;

    
    private int len;

    
    Hyphenation(String word, int[] points) {
        this.word = word;
        hyphenPoints = points;
        len = points.length;
    }

    
    public int length() {
        return len;
    }

    
    public String getPreHyphenText(int index) {
        return word.substring(0, hyphenPoints[index]);
    }

    
    public String getPostHyphenText(int index) {
        return word.substring(hyphenPoints[index]);
    }

    
    public int[] getHyphenationPoints() {
        return hyphenPoints;
    }

    public String toString() {
        StringBuffer str = new StringBuffer();
        int start = 0;
        for (int i = 0; i < len; i++) {
            str.append(word.substring(start, hyphenPoints[i])).append('-');
            start = hyphenPoints[i];
        }
        str.append(word.substring(start));
        return str.toString();
    }

}
