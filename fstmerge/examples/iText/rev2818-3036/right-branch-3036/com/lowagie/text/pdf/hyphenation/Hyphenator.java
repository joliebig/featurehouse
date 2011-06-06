

package com.lowagie.text.pdf.hyphenation;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Hashtable;

import com.lowagie.text.pdf.BaseFont;


public class Hyphenator {
    
    
    private static Hashtable<String, HyphenationTree> hyphenTrees = new Hashtable<String, HyphenationTree>();

    private HyphenationTree hyphenTree = null;
    private int remainCharCount = 2;
    private int pushCharCount = 2;
    private static final String defaultHyphLocation = "com/lowagie/text/pdf/hyphenation/hyph/";
   
    
    private static String hyphenDir = "";    

    
    public Hyphenator(String lang, String country, int leftMin,
                      int rightMin) {
        hyphenTree = getHyphenationTree(lang, country);
        remainCharCount = leftMin;
        pushCharCount = rightMin;
    }

    
    public static HyphenationTree getHyphenationTree(String lang,
            String country) {
        String key = lang;
        
        if (country != null && !country.equals("none")) {
            key += "_" + country;
        }
            
        if (hyphenTrees.containsKey(key)) {
            return hyphenTrees.get(key);
        }
        if (hyphenTrees.containsKey(lang)) {
            return hyphenTrees.get(lang);
        }

        HyphenationTree hTree = getResourceHyphenationTree(key);
        if (hTree == null)
            hTree = getFileHyphenationTree(key);
        
        if (hTree != null) {
            hyphenTrees.put(key, hTree);
        }
        return hTree;
    }

    
    public static HyphenationTree getResourceHyphenationTree(String key) {
        try {
            InputStream stream = BaseFont.getResourceStream(defaultHyphLocation + key + ".xml");
            if (stream == null && key.length() > 2)
                stream = BaseFont.getResourceStream(defaultHyphLocation + key.substring(0, 2) + ".xml");
            if (stream == null)
                return null;
            HyphenationTree hTree = new HyphenationTree();
            hTree.loadSimplePatterns(stream);
            return hTree;
        }
        catch (Exception e) {
            return null;
        }
    }

    
    public static HyphenationTree getFileHyphenationTree(String key) {
        try {
            if (hyphenDir == null)
                return null;
            InputStream stream = null;
            File hyphenFile = new File(hyphenDir, key + ".xml");
            if (hyphenFile.canRead())
                stream = new FileInputStream(hyphenFile);
            if (stream == null && key.length() > 2) {
                hyphenFile = new File(hyphenDir, key.substring(0, 2) + ".xml");
                if (hyphenFile.canRead())
                    stream = new FileInputStream(hyphenFile);
            }
            if (stream == null)
                return null;
            HyphenationTree hTree = new HyphenationTree();
            hTree.loadSimplePatterns(stream);
            return hTree;
        }
        catch (Exception e) {
            return null;
        }
    }

    
    public static Hyphenation hyphenate(String lang, String country,
                                        String word, int leftMin,
                                        int rightMin) {
        HyphenationTree hTree = getHyphenationTree(lang, country);
        if (hTree == null) {
            
            
            return null;
        }
        return hTree.hyphenate(word, leftMin, rightMin);
    }

    
    public static Hyphenation hyphenate(String lang, String country,
                                        char[] word, int offset, int len,
                                        int leftMin, int rightMin) {
        HyphenationTree hTree = getHyphenationTree(lang, country);
        if (hTree == null) {
            
            
            return null;
        }
        return hTree.hyphenate(word, offset, len, leftMin, rightMin);
    }

    
    public void setMinRemainCharCount(int min) {
        remainCharCount = min;
    }

    
    public void setMinPushCharCount(int min) {
        pushCharCount = min;
    }

    
    public void setLanguage(String lang, String country) {
        hyphenTree = getHyphenationTree(lang, country);
    }

    
    public Hyphenation hyphenate(char[] word, int offset, int len) {
        if (hyphenTree == null) {
            return null;
        }
        return hyphenTree.hyphenate(word, offset, len, remainCharCount,
                                    pushCharCount);
    }

    
    public Hyphenation hyphenate(String word) {
        if (hyphenTree == null) {
            return null;
        }
        return hyphenTree.hyphenate(word, remainCharCount, pushCharCount);
    }

    
    public static String getHyphenDir() {
        return hyphenDir;
    }
    
    
    public static void setHyphenDir(String _hyphenDir) {
        hyphenDir = _hyphenDir;
    }
    
}
