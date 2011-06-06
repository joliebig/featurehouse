


 
package com.lowagie.text.pdf.hyphenation;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;


public class HyphenationTree extends TernaryTree 
            implements PatternConsumer {

    private static final long serialVersionUID = -7763254239309429432L;

    
    protected ByteVector vspace;

    
    protected HashMap<String, ArrayList<Object>> stoplist;

    
    protected TernaryTree classmap;

    
    private transient TernaryTree ivalues;

    public HyphenationTree() {
        stoplist = new HashMap<String, ArrayList<Object>>(23);    
        classmap = new TernaryTree();
        vspace = new ByteVector();
        vspace.alloc(1);    
    }

    
    protected int packValues(String values) {
        int i, n = values.length();
        int m = (n & 1) == 1 ? (n >> 1) + 2 : (n >> 1) + 1;
        int offset = vspace.alloc(m);
        byte[] va = vspace.getArray();
        for (i = 0; i < n; i++) {
            int j = i >> 1;
            byte v = (byte)((values.charAt(i) - '0' + 1) & 0x0f);
            if ((i & 1) == 1) {
                va[j + offset] = (byte)(va[j + offset] | v);
            } else {
                va[j + offset] = (byte)(v << 4);    
            }
        }
        va[m - 1 + offset] = 0;    
        return offset;
    }

    protected String unpackValues(int k) {
        StringBuffer buf = new StringBuffer();
        byte v = vspace.get(k++);
        while (v != 0) {
            char c = (char)((v >>> 4) - 1 + '0');
            buf.append(c);
            c = (char)(v & 0x0f);
            if (c == 0) {
                break;
            }
            c = (char)(c - 1 + '0');
            buf.append(c);
            v = vspace.get(k++);
        }
        return buf.toString();
    }

    public void loadSimplePatterns(InputStream stream) {
        SimplePatternParser pp = new SimplePatternParser();
        ivalues = new TernaryTree();

        pp.parse(stream, this);

        
        
        trimToSize();
        vspace.trimToSize();
        classmap.trimToSize();

        
        ivalues = null;
    }


    public String findPattern(String pat) {
        int k = super.find(pat);
        if (k >= 0) {
            return unpackValues(k);
        }
        return "";
    }

    
    protected int hstrcmp(char[] s, int si, char[] t, int ti) {
        for (; s[si] == t[ti]; si++, ti++) {
            if (s[si] == 0) {
                return 0;
            }
        }
        if (t[ti] == 0) {
            return 0;
        }
        return s[si] - t[ti];
    }

    protected byte[] getValues(int k) {
        StringBuffer buf = new StringBuffer();
        byte v = vspace.get(k++);
        while (v != 0) {
            char c = (char)((v >>> 4) - 1);
            buf.append(c);
            c = (char)(v & 0x0f);
            if (c == 0) {
                break;
            }
            c = (char)(c - 1);
            buf.append(c);
            v = vspace.get(k++);
        }
        byte[] res = new byte[buf.length()];
        for (int i = 0; i < res.length; i++) {
            res[i] = (byte)buf.charAt(i);
        }
        return res;
    }

    
    protected void searchPatterns(char[] word, int index, byte[] il) {
        byte[] values;
        int i = index;
        char p, q;
        char sp = word[i];
        p = root;

        while (p > 0 && p < sc.length) {
            if (sc[p] == 0xFFFF) {
                if (hstrcmp(word, i, kv.getArray(), lo[p]) == 0) {
                    values = getValues(eq[p]);    
                    int j = index;
                    for (int k = 0; k < values.length; k++) {
                        if (j < il.length && values[k] > il[j]) {
                            il[j] = values[k];
                        }
                        j++;
                    }
                }
                return;
            }
            int d = sp - sc[p];
            if (d == 0) {
                if (sp == 0) {
                    break;
                }
                sp = word[++i];
                p = eq[p];
                q = p;

                
                
                while (q > 0 && q < sc.length) {
                    if (sc[q] == 0xFFFF) {        
                        break;
                    }
                    if (sc[q] == 0) {
                        values = getValues(eq[q]);
                        int j = index;
                        for (int k = 0; k < values.length; k++) {
                            if (j < il.length && values[k] > il[j]) {
                                il[j] = values[k];
                            }
                            j++;
                        }
                        break;
                    } else {
                        q = lo[q];

                        
                    }
                }
            } else {
                p = d < 0 ? lo[p] : hi[p];
            }
        }
    }

    
    public Hyphenation hyphenate(String word, int remainCharCount,
                                 int pushCharCount) {
        char[] w = word.toCharArray();
        return hyphenate(w, 0, w.length, remainCharCount, pushCharCount);
    }

    

    
    public Hyphenation hyphenate(char[] w, int offset, int len,
                                 int remainCharCount, int pushCharCount) {
        int i;
        char[] word = new char[len + 3];

        
        char[] c = new char[2];
        int iIgnoreAtBeginning = 0;
        int iLength = len;
        boolean bEndOfLetters = false;
        for (i = 1; i <= len; i++) {
            c[0] = w[offset + i - 1];
            int nc = classmap.find(c, 0);
            if (nc < 0) {    
                if (i == (1 + iIgnoreAtBeginning)) {
                    
                    iIgnoreAtBeginning ++;
                } else {
                    
                    bEndOfLetters = true;
                }
                iLength --;
            } else {
                if (!bEndOfLetters) {
                    word[i - iIgnoreAtBeginning] = (char)nc;
                } else {
                    return null;
                }
            }
        }
        len = iLength;
        if (len < (remainCharCount + pushCharCount)) {
            
            return null;
        }
        int[] result = new int[len + 1];
        int k = 0;

        
        String sw = new String(word, 1, len);
        if (stoplist.containsKey(sw)) {
            
            ArrayList<Object> hw = stoplist.get(sw);
            int j = 0;
            for (i = 0; i < hw.size(); i++) {
                Object o = hw.get(i);
                
                
                if (o instanceof String) {
                    j += ((String)o).length();
                    if (j >= remainCharCount && j < (len - pushCharCount)) {
                        result[k++] = j + iIgnoreAtBeginning;
                    }
                }
            }
        } else {
            
            word[0] = '.';                    
            word[len + 1] = '.';              
            word[len + 2] = 0;                
            byte[] il = new byte[len + 3];    
            for (i = 0; i < len + 1; i++) {
                searchPatterns(word, i, il);
            }

            
            
            
            
            for (i = 0; i < len; i++) {
                if (((il[i + 1] & 1) == 1) && i >= remainCharCount
                        && i <= (len - pushCharCount)) {
                    result[k++] = i + iIgnoreAtBeginning;
                }
            }
        }


        if (k > 0) {
            
            int[] res = new int[k];
            System.arraycopy(result, 0, res, 0, k);
            return new Hyphenation(new String(w, offset, len), res);
        } else {
            return null;
        }
    }

    
    public void addClass(String chargroup) {
        if (chargroup.length() > 0) {
            char equivChar = chargroup.charAt(0);
            char[] key = new char[2];
            key[1] = 0;
            for (int i = 0; i < chargroup.length(); i++) {
                key[0] = chargroup.charAt(i);
                classmap.insert(key, 0, equivChar);
            }
        }
    }

    
    public void addException(String word, ArrayList<Object> hyphenatedword) {
        stoplist.put(word, hyphenatedword);
    }

    
    public void addPattern(String pattern, String ivalue) {
        int k = ivalues.find(ivalue);
        if (k <= 0) {
            k = packValues(ivalue);
            ivalues.insert(ivalue, (char)k);
        }
        insert(pattern, (char)k);
    }

    public void printStats() {
        System.out.println("Value space size = "
                           + Integer.toString(vspace.length()));
        super.printStats();
    }
}
