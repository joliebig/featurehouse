

package com.lowagie.text.pdf;

import java.io.IOException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Utilities;


class TrueTypeFontUnicode extends TrueTypeFont implements Comparator{
    
        
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
    
    
    public int getWidth(int char1) {
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
            for (int k = 0; k < len; ++k) {
                if (Utilities.isSurrogatePair(text, k)) {
                    total += getRawWidth(Utilities.convertToUtf32(text, k), encoding);
                    ++k;
                }
                else
                    total += getRawWidth(text.charAt(k), encoding);
            }
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
        "<< /Registry (TTX+0)\n" +
        "/Ordering (T42UV)\n" +
        "/Supplement 0\n" +
        ">> def\n" +
        "/CMapName /TTX+0 def\n" +
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
    
    private static String toHex4(int n) {
        String s = "0000" + Integer.toHexString(n);
        return s.substring(s.length() - 4);
    }
    
        
    static String toHex(int n) {
        if (n < 0x10000)
            return "<" + toHex4(n) + ">";
        n -= 0x10000;
        int high = (n / 0x400) + 0xd800;
        int low = (n % 0x400) + 0xdc00;
        return "[<" + toHex4(high) + toHex4(low) + ">]";
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

        
    public int compare(Object o1, Object o2) {
        int m1 = ((int[])o1)[0];
        int m2 = ((int[])o2)[0];
        if (m1 < m2)
            return -1;
        if (m1 == m2)
            return 0;
        return 1;
    }
    
    private static final byte[] rotbits = {(byte)0x80,(byte)0x40,(byte)0x20,(byte)0x10,(byte)0x08,(byte)0x04,(byte)0x02,(byte)0x01};
    
    
    void writeFont(PdfWriter writer, PdfIndirectReference ref, Object params[]) throws DocumentException, IOException {
        HashMap longTag = (HashMap)params[0];
        addRangeUni(longTag, true, subset);
        Object metrics[] = longTag.values().toArray();
        Arrays.sort(metrics, this);
        PdfIndirectReference ind_font = null;
        PdfObject pobj = null;
        PdfIndirectObject obj = null;
        PdfIndirectReference cidset = null;
        if (writer.getPDFXConformance() == PdfWriter.PDFA1A || writer.getPDFXConformance() == PdfWriter.PDFA1B) {
            PdfStream stream;
            if (metrics.length == 0) {
                stream = new PdfStream(new byte[]{(byte)0x80});
            }
            else {
                int top = ((int[])metrics[metrics.length - 1])[0];
                byte[] bt = new byte[top / 8 + 1];
                for (int k = 0; k < metrics.length; ++k) {
                    int v = ((int[])metrics[k])[0];
                    bt[v / 8] |= rotbits[v % 8];
                }
                stream = new PdfStream(bt);
                stream.flateCompress();
            }
            cidset = writer.addToBody(stream).getIndirectReference();
        }
        
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
                TrueTypeFontSubSet sb = new TrueTypeFontSubSet(fileName, new RandomAccessFileOrArray(rf), longTag, directoryOffset, false, false);
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
        PdfDictionary dic = getFontDescriptor(ind_font, subsetPrefix, cidset);
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

    byte[] convertToBytes(int char1) {
        return null;
    }

        
    public int[] getMetricsTT(int c) {
        if (cmapExt != null)
            return (int[])cmapExt.get(new Integer(c));
        HashMap map = null;
        if (fontSpecific)
            map = cmap10;
        else
            map = cmap31;
        if (map == null)
            return null;
        if (fontSpecific) {
            if ((c & 0xffffff00) == 0 || (c & 0xffffff00) == 0xf000)
                return (int[])map.get(new Integer(c & 0xff));
            else
                return null;
        }
        else
            return (int[])map.get(new Integer(c));
    }
    
    
    public boolean charExists(int c) {
        return getMetricsTT(c) != null;
    }
    
    
    public boolean setCharAdvance(int c, int advance) {
        int[] m = getMetricsTT(c);
        if (m == null)
            return false;
        m[1] = advance;
        return true;
    }
    
    public int[] getCharBBox(char c) {
        if (bboxes == null)
            return null;
        int[] m = getMetricsTT(c);
        if (m == null)
            return null;
        return bboxes[m[0]];
    }
}
