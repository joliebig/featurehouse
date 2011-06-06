
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

    private static String[] excUriEsc = {"%20", "%3C", "%3E", "%23", "%25", "%22", "%7B", "%7D", "%5B", "%5D", "%7C", "%5C", "%5E", "%60"};
    private static String excUri = " <>#%\"{}[]|\\\u\u";
    
    public static URL toURL(String filename) throws MalformedURLException {
        if (filename.startsWith("file:/") || filename.startsWith("http://")
                || filename.startsWith("https://")
                || filename.startsWith("jar:")) {
            return new URL(filename);
        }
        File f = new File(filename);
        String path = f.getAbsolutePath();
        if (File.separatorChar != '/') {
            path = path.replace(File.separatorChar, '/');
        }
        if (!path.startsWith("/")) {
            path = "/" + path;
        }
        if (!path.endsWith("/") && f.isDirectory()) {
            path = path + "/";
        }
        char[] t = path.toCharArray();
        StringBuffer sb = new StringBuffer();
        for (int k = 0; k < t.length; ++k) {
            char c = t[k];
            int a = Utilities.excUri.indexOf(c);
            if (a >= 0)
                sb.append(Utilities.excUriEsc[a]);
            else
                sb.append(c);
        }
        return new URL("file", "", sb.toString());
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

}
