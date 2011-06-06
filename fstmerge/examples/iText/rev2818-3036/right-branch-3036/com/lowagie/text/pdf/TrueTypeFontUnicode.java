

package com.lowagie.text.pdf;

import java.io.IOException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;

import com.lowagie.text.DocumentException;

class TrueTypeFontUnicode extends TrueTypeFont implements Comparator<int[]>{
    
        
    boolean vertical = false;

    
    TrueTypeFontUnicode(String ttFile, String enc, boolean emb, byte ttfAfm[]) throws DocumentException, IOException {
        String nameBase = getBaseName(ttFile);
        String ttcName = getTTCName(nameBase);
        if (nameBase.length() < ttFile.length()) {
            style = ttFile.substring(nameBase.length());
        }
        encoding = enc;
        embedded = emb;
        fileName = ttcName;
        ttcIndex = "";
        if (ttcName.length() < nameBase.length())
            ttcIndex = nameBase.substring(ttcName.length() + 1);
        fontType = FONT_TYPE_TTUNI;
        if ((fileName.toLowerCase().endsWith(".ttf") || fileName.toLowerCase().endsWith(".otf") || fileName.toLowerCase().endsWith(".ttc")) && ((enc.equals(IDENTITY_H) || enc.equals(IDENTITY_V)) && emb)) {
            process(ttfAfm);
            if (os_2.fsType == 2)
                throw new DocumentException(fileName + style + " cannot be embedded due to licensing restrictions.");
            
            if ((cmap31 == null && !fontSpecific) || (cmap10 == null && fontSpecific))
                directTextToByte=true;
                
            if (fontSpecific) {
                fontSpecific = false;
                String tempEncoding = encoding;
                encoding = "";
                createEncoding();
                encoding = tempEncoding;
                fontSpecific = true;
            }
        }
        else
            throw new DocumentException(fileName + " " + style + " is not a TTF font file.");
        vertical = enc.endsWith("V");
    }
    
    
    public int getWidth(char char1) {
        if (vertical)
            return 1000;
        if (fontSpecific) {
            if ((char1 & 0xff00) == 0 || (char1 & 0xff00) == 0xf000)
                return getRawWidth(char1 & 0xff, null);
            else
                return 0;
        }
        else {
            return getRawWidth(char1, encoding);
        }
    }
    
    
    public int getWidth(String text) {
        if (vertical)
            return text.length() * 1000;
        int total = 0;
        if (fontSpecific) {
            char cc[] = text.toCharArray();
            int len = cc.length;
            for (int k = 0; k < len; ++k) {
                char c = cc[k];
                if ((c & 0xff00) == 0 || (c & 0xff00) == 0xf000)
                    total += getRawWidth(c & 0xff, null);
            }
        }
        else {
            int len = text.length();
            for (int k = 0; k < len; ++k)
                total += getRawWidth(text.charAt(k), encoding);
        }
        return total;
    }

        
    private PdfStream getToUnicode(Object metrics[]) {
        if (metrics.length == 0)
            return null;
        StringBuffer buf = new StringBuffer(
        "/CIDInit /ProcSet findresource begin\n" +
        "12 dict begin\n" +
        "begincmap\n" +
        "/CIDSystemInfo\n" +
        "<< /Registry (Adobe)\n" +
        "/Ordering (UCS)\n" +
        "/Supplement 0\n" +
        ">> def\n" +
        "/CMapName /Adobe-Identity-UCS def\n" +
        "/CMapType 2 def\n" +
        "1 begincodespacerange\n" +
        "<0000><FFFF>\n" +
        "endcodespacerange\n");
        int size = 0;
        for (int k = 0; k < metrics.length; ++k) {
            if (size == 0) {
                if (k != 0) {
                    buf.append("endbfrange\n");
                }
                size = Math.min(100, metrics.length - k);
                buf.append(size).append(" beginbfrange\n");
            }
            --size;
            int metric[] = (int[])metrics[k];
            String fromTo = toHex(metric[0]);
            buf.append(fromTo).append(fromTo).append(toHex(metric[2])).append('\n');
        }
        buf.append(
        "endbfrange\n" +
        "endcmap\n" +
        "CMapName currentdict /CMap defineresource pop\n" +
        "end end\n");
        String s = buf.toString();
        PdfStream stream = new PdfStream(PdfEncodings.convertToBytes(s, null));
        stream.flateCompress();
        return stream;
    }
    
        
    static String toHex(int n) {
        String s = Integer.toHexString(n);
        return "<0000".substring(0, 5 - s.length()) + s + ">";
    }
    
        
    private PdfDictionary getCIDFontType2(PdfIndirectReference fontDescriptor, String subsetPrefix, Object metrics[]) {
        PdfDictionary dic = new PdfDictionary(PdfName.FONT);
        
        if (cff) {
            dic.put(PdfName.SUBTYPE, PdfName.CIDFONTTYPE0);
            dic.put(PdfName.BASEFONT, new PdfName(subsetPrefix+fontName+"-"+encoding));
        }
        else {
            dic.put(PdfName.SUBTYPE, PdfName.CIDFONTTYPE2);
            dic.put(PdfName.BASEFONT, new PdfName(subsetPrefix + fontName));
        }
        dic.put(PdfName.FONTDESCRIPTOR, fontDescriptor);
        if (!cff)
          dic.put(PdfName.CIDTOGIDMAP,PdfName.IDENTITY);
        PdfDictionary cdic = new PdfDictionary();
        cdic.put(PdfName.REGISTRY, new PdfString("Adobe"));
        cdic.put(PdfName.ORDERING, new PdfString("Identity"));
        cdic.put(PdfName.SUPPLEMENT, new PdfNumber(0));
        dic.put(PdfName.CIDSYSTEMINFO, cdic);
        if (!vertical) {
            dic.put(PdfName.DW, new PdfNumber(1000));
            StringBuffer buf = new StringBuffer("[");
            int lastNumber = -10;
            boolean firstTime = true;
            for (int k = 0; k < metrics.length; ++k) {
                int metric[] = (int[])metrics[k];
                if (metric[1] == 1000)
                    continue;
                int m = metric[0];
                if (m == lastNumber + 1) {
                    buf.append(' ').append(metric[1]);
                }
                else {
                    if (!firstTime) {
                        buf.append(']');
                    }
                    firstTime = false;
                    buf.append(m).append('[').append(metric[1]);
                }
                lastNumber = m;
            }
            if (buf.length() > 1) {
                buf.append("]]");
                dic.put(PdfName.W, new PdfLiteral(buf.toString()));
            }
        }
        return dic;
    }
    
        
    private PdfDictionary getFontBaseType(PdfIndirectReference descendant, String subsetPrefix, PdfIndirectReference toUnicode) {
        PdfDictionary dic = new PdfDictionary(PdfName.FONT);

        dic.put(PdfName.SUBTYPE, PdfName.TYPE0);
        
        if (cff)
          dic.put(PdfName.BASEFONT, new PdfName(subsetPrefix+fontName+"-"+encoding));
          
        else
          dic.put(PdfName.BASEFONT, new PdfName(subsetPrefix + fontName));
          
        dic.put(PdfName.ENCODING, new PdfName(encoding));
        dic.put(PdfName.DESCENDANTFONTS, new PdfArray(descendant));
        if (toUnicode != null)
            dic.put(PdfName.TOUNICODE, toUnicode);  
        return dic;
    }

        
    public int compare(int[] o1, int[] o2) {
        int m1 = o1[0];
        int m2 = o2[0];
        if (m1 < m2)
            return -1;
        if (m1 == m2)
            return 0;
        return 1;
    }
    
    
    void writeFont(PdfWriter writer, PdfIndirectReference ref, Object params[]) throws DocumentException, IOException {
        HashMap<Integer, int[]> longTag = (HashMap<Integer, int[]>)params[0];
        addRangeUni(longTag, subset);
        int metrics[][] = longTag.values().toArray(new int[0][]);
        Arrays.sort(metrics, this);
        PdfIndirectReference ind_font = null;
        PdfObject pobj = null;
        PdfIndirectObject obj = null;
        
        if (cff) {
            RandomAccessFileOrArray rf2 = new RandomAccessFileOrArray(rf);
            byte b[] = new byte[cffLength];
            try {
                rf2.reOpen();
                rf2.seek(cffOffset);
                rf2.readFully(b);
            } finally {
                try {
                    rf2.close();
                } catch (Exception e) {
                    
                }
            }
            if (subset || subsetRanges != null) {
                CFFFontSubset cff = new CFFFontSubset(new RandomAccessFileOrArray(b),longTag);
                b = cff.Process( (cff.getNames())[0] );
            }
            pobj = new StreamFont(b, "CIDFontType0C");
            obj = writer.addToBody(pobj);
            ind_font = obj.getIndirectReference();
        } else {
            byte[] b;
            if (subset || directoryOffset != 0) {
                TrueTypeFontSubSet sb = new TrueTypeFontSubSet(fileName, new RandomAccessFileOrArray(rf), new HashSet<Integer>(longTag.keySet()), directoryOffset, false, false);
                b = sb.process();
            }
            else {
                b = getFullFont();
            }
            int lengths[] = new int[]{b.length};
            pobj = new StreamFont(b, lengths);
            obj = writer.addToBody(pobj);
            ind_font = obj.getIndirectReference();
        }
        String subsetPrefix = "";
        if (subset)
            subsetPrefix = createSubsetPrefix();
        PdfDictionary dic = getFontDescriptor(ind_font, subsetPrefix);
        obj = writer.addToBody(dic);
        ind_font = obj.getIndirectReference();

        pobj = getCIDFontType2(ind_font, subsetPrefix, metrics);
        obj = writer.addToBody(pobj);
        ind_font = obj.getIndirectReference();

        pobj = getToUnicode(metrics);
        PdfIndirectReference toUnicodeRef = null;
        
        if (pobj != null) {
            obj = writer.addToBody(pobj);
            toUnicodeRef = obj.getIndirectReference();
        }

        pobj = getFontBaseType(ind_font, subsetPrefix, toUnicodeRef);
        writer.addToBody(pobj, ref);
    }

        
    byte[] convertToBytes(String text) {
        return null;
    }

    byte[] convertToBytes(char char1) {
        return null;
    }

    
    public boolean charExists(char c) {
        HashMap<Integer, int[]> map = null;
        if (fontSpecific)
            map = cmap10;
        else
            map = cmap31;
        if (map == null)
            return false;
        if (fontSpecific) {
            if ((c & 0xff00) == 0 || (c & 0xff00) == 0xf000)
                return map.get(new Integer(c & 0xff)) != null;
            else
                return false;
        }
        else
            return map.get(new Integer(c)) != null;
    }
    
    
    public boolean setCharAdvance(char c, int advance) {
        HashMap<Integer, int[]> map = null;
        if (fontSpecific)
            map = cmap10;
        else
            map = cmap31;
        if (map == null)
            return false;
        int m[] = null;
        if (fontSpecific) {
            if ((c & 0xff00) == 0 || (c & 0xff00) == 0xf000)
                m = map.get(new Integer(c & 0xff));
            else
                return false;
        }
        else
            m = map.get(new Integer(c));
        if (m == null)
            return false;
        else
            m[1] = advance;
        return true;
    }
    
    public int[] getCharBBox(char c) {
        if (bboxes == null)
            return null;
        HashMap<Integer, int[]> map = null;
        if (fontSpecific)
            map = cmap10;
        else
            map = cmap31;
        if (map == null)
            return null;
        int m[] = null;
        if (fontSpecific) {
            if ((c & 0xff00) == 0 || (c & 0xff00) == 0xf000)
                m = map.get(new Integer(c & 0xff));
            else
                return null;
        }
        else
            m = map.get(new Integer(c));
        if (m == null)
            return null;
        return bboxes[m[0]];
    }
}
