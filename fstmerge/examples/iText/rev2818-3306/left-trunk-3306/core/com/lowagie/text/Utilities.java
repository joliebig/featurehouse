
package com.lowagie.text;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.Hashtable;
import java.util.Properties;
import java.util.Set;

import com.lowagie.text.pdf.PRTokeniser;



public class Utilities {

    
    public static Set getKeySet(Hashtable table) {
        return (table == null) ? Collections.EMPTY_SET : table.keySet();
    }

    
    public static Object[][] addToArray(Object original[][], Object item[]) {
        if (original == null) {
            original = new Object[1][];
            original[0] = item;
            return original;
        } else {
            Object original2[][] = new Object[original.length + 1][];
            System.arraycopy(original, 0, original2, 0, original.length);
            original2[original.length] = item;
            return original2;
        }
    }

    
    public static boolean checkTrueOrFalse(Properties attributes, String key) {
        return "true".equalsIgnoreCase(attributes.getProperty(key));
    }

        
    public static String unEscapeURL(String src) {
        StringBuffer bf = new StringBuffer();
        char[] s = src.toCharArray();
        for (int k = 0; k < s.length; ++k) {
            char c = s[k];
            if (c == '%') {
                if (k + 2 >= s.length) {
                    bf.append(c);
                    continue;
                }
                int a0 = PRTokeniser.getHex((int)s[k + 1]);
                int a1 = PRTokeniser.getHex((int)s[k + 2]);
                if (a0 < 0 || a1 < 0) {
                    bf.append(c);
                    continue;
                }
                bf.append((char)(a0 * 16 + a1));
                k += 2;
            }
            else
                bf.append(c);
        }
        return bf.toString();
    }

    
    public static URL toURL(String filename) throws MalformedURLException {
        try {
            return new URL(filename);
        }
        catch (Exception e) {
            return new File(filename).toURI().toURL();
        }
    }

    
    static public void skip(InputStream is, int size) throws IOException {
        long n;
        while (size > 0) {
            n = is.skip(size);
            if (n <= 0)
                break;
            size -= n;
        }
    }

    
    public static final float cm2pt(float value) {
        return millimetersToPoints(value * 10f);
    }

    
    public static final float cm2i(float value) {
        return millimetersToPoints(value * 10f);
    }

    
    public static final float pt2cm(float value) {
        return pointsToMillimeters(value) / 10f;
    }

    
    public static final float pt2i(float value) {
        return pointsToInches(value);
    }

    
    public static final float i2cm(float value) {
        return inchesToMillimeters(value) / 10;
    }

    
    public static final float i2pt(float value) {
        return inchesToPoints(value);
    }

    
    public static final float millimetersToPoints(float value) {
        return inchesToPoints(millimetersToInches(value));
    }

    
    public static final float millimetersToInches(float value) {
        return value / 25.4f;
    }

    
    public static final float pointsToMillimeters(float value) {
        return inchesToMillimeters(pointsToInches(value));
    }

    
    public static final float pointsToInches(float value) {
        return value / 72f;
    }

    
    public static final float inchesToMillimeters(float value) {
        return value * 25.4f;
    }

    
    public static final float inchesToPoints(float value) {
        return value * 72f;
    }
}
