
package com.lowagie.text.pdf;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;


public class DocumentFont extends BaseFont {
    
    private HashMap<Integer, int[]> metrics = new HashMap<Integer, int[]>();
    private String fontName;
    private PRIndirectReference refFont;
    private PdfDictionary font;
    private IntHashtable uni2byte = new IntHashtable();
    private IntHashtable byte2uni = new IntHashtable();
    private float Ascender = 800;
    private float CapHeight = 700;
    private float Descender = -200;
    private float ItalicAngle = 0;
    private float llx = -50;
    private float lly = -200;
    private float urx = 100;
    private float ury = 900;
    private boolean isType0 = false;
    
    private BaseFont cjkMirror;
    
    private static String cjkNames[] = {"HeiseiMin-W3", "HeiseiKakuGo-W5", "STSong-Light", "MHei-Medium",
        "MSung-Light", "HYGoThic-Medium", "HYSMyeongJo-Medium", "MSungStd-Light", "STSongStd-Light",
        "HYSMyeongJoStd-Medium", "KozMinPro-Regular"};
        
    private static String cjkEncs[] = {"UniJIS-UCS2-H", "UniJIS-UCS2-H", "UniGB-UCS2-H", "UniCNS-UCS2-H",
        "UniCNS-UCS2-H", "UniKS-UCS2-H", "UniKS-UCS2-H", "UniCNS-UCS2-H", "UniGB-UCS2-H",
        "UniKS-UCS2-H", "UniJIS-UCS2-H"};
        
    private static String cjkNames2[] = {"MSungStd-Light", "STSongStd-Light", "HYSMyeongJoStd-Medium", "KozMinPro-Regular"};
        
    private static String cjkEncs2[] = {"UniCNS-UCS2-H", "UniGB-UCS2-H", "UniKS-UCS2-H", "UniJIS-UCS2-H",
        "UniCNS-UTF16-H", "UniGB-UTF16-H", "UniKS-UTF16-H", "UniJIS-UTF16-H"};
        
    private static final int stdEnc[] = {
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        32,33,34,35,36,37,38,8217,40,41,42,43,44,45,46,47,
        48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,
        64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
        80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,
        8216,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,
        112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,161,162,163,8260,165,402,167,164,39,8220,171,8249,8250,64257,64258,
        0,8211,8224,8225,183,0,182,8226,8218,8222,8221,187,8230,8240,0,191,
        0,96,180,710,732,175,728,729,168,0,730,184,0,733,731,711,
        8212,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,198,0,170,0,0,0,0,321,216,338,186,0,0,0,0,
        0,230,0,0,0,305,0,0,322,248,339,223,0,0,0,0};

    
    DocumentFont(PRIndirectReference refFont) {
        encoding = "";
        fontSpecific = false;
        this.refFont = refFont;
        fontType = FONT_TYPE_DOCUMENT;
        font = (PdfDictionary)PdfReader.getPdfObject(refFont);
        fontName = PdfName.decodeName(((PdfName)PdfReader.getPdfObject(font.get(PdfName.BASEFONT))).toString());
        PdfName subType = (PdfName)PdfReader.getPdfObject(font.get(PdfName.SUBTYPE));
        if (PdfName.TYPE1.equals(subType) || PdfName.TRUETYPE.equals(subType))
            doType1TT();
        else {
            for (int k = 0; k < cjkNames.length; ++k) {
                if (fontName.startsWith(cjkNames[k])) {
                    fontName = cjkNames[k];
                    try {
                        cjkMirror = BaseFont.createFont(fontName, cjkEncs[k], false);
                    }
                    catch (Exception e) {
                        throw new ExceptionConverter(e);
                    }
                    return;
                }
            }
            String enc = PdfName.decodeName(((PdfName)PdfReader.getPdfObject(font.get(PdfName.ENCODING))).toString());
            for (int k = 0; k < cjkEncs2.length; ++k) {
                if (enc.startsWith(cjkEncs2[k])) {
                    try {
                        if (k > 3)
                            k -= 4;
                        cjkMirror = BaseFont.createFont(cjkNames2[k], cjkEncs2[k], false);
                    }
                    catch (Exception e) {
                        throw new ExceptionConverter(e);
                    }
                    return;
                }
            }
            if (PdfName.TYPE0.equals(subType) && enc.equals("Identity-H")) {
                processType0(font);
                isType0 = true;
            }
        }
    }
    
    private void processType0(PdfDictionary font) {
        try {
            byte[] touni = PdfReader.getStreamBytes((PRStream)PdfReader.getPdfObjectRelease(font.get(PdfName.TOUNICODE)));
            PdfArray df = (PdfArray)PdfReader.getPdfObjectRelease(font.get(PdfName.DESCENDANTFONTS));
            PdfDictionary cidft = (PdfDictionary)PdfReader.getPdfObjectRelease(df.getArrayList().get(0));
            PdfNumber dwo = (PdfNumber)PdfReader.getPdfObjectRelease(cidft.get(PdfName.DW));
            int dw = 1000;
            if (dwo != null)
                dw = dwo.intValue();
            IntHashtable widths = readWidths((PdfArray)PdfReader.getPdfObjectRelease(cidft.get(PdfName.W)));
            PdfDictionary fontDesc = (PdfDictionary)PdfReader.getPdfObjectRelease(cidft.get(PdfName.FONTDESCRIPTOR));
            fillFontDesc(fontDesc);
            fillMetrics(touni, widths, dw);
        } catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
    private IntHashtable readWidths(PdfArray ws) {
        IntHashtable hh = new IntHashtable();
        if (ws == null)
            return hh;
        ArrayList<PdfObject> ar = ws.getArrayList();
        for (int k = 0; k < ar.size(); ++k) {
            int c1 = ((PdfNumber)PdfReader.getPdfObjectRelease(ar.get(k))).intValue();
            PdfObject obj = PdfReader.getPdfObjectRelease(ar.get(++k));
            if (obj.isArray()) {
                ArrayList<PdfObject> ar2 = ((PdfArray)obj).getArrayList();
                for (int j = 0; j < ar2.size(); ++j) {
                    int c2 = ((PdfNumber)PdfReader.getPdfObjectRelease(ar2.get(j))).intValue();
                    hh.put(c1++, c2);
                }
            }
            else {
                int c2 = ((PdfNumber)obj).intValue();
                int w = ((PdfNumber)PdfReader.getPdfObjectRelease(ar.get(++k))).intValue();
                for (; c1 <= c2; ++c1)
                    hh.put(c1, w);
            }
        }
        return hh;
    }
    
    private String decodeString(PdfString ps) {
        if (ps.isHexWriting())
            return PdfEncodings.convertToString(ps.getBytes(), "UnicodeBigUnmarked");
        else
            return ps.toUnicodeString();
    }
    
    private void fillMetrics(byte[] touni, IntHashtable widths, int dw) {
        try {
            PdfContentParser ps = new PdfContentParser(new PRTokeniser(touni));
            PdfObject ob = null;
            PdfObject last = null;
            while ((ob = ps.readPRObject()) != null) {
                if (ob.type() == PdfContentParser.COMMAND_TYPE) {
                    if (ob.toString().equals("beginbfchar")) {
                        int n = ((PdfNumber)last).intValue();
                        for (int k = 0; k < n; ++k) {
                            String cid = decodeString((PdfString)ps.readPRObject());
                            String uni = decodeString((PdfString)ps.readPRObject());
                            if (uni.length() == 1) {
                                int cidc = cid.charAt(0);
                                int unic = uni.charAt(uni.length() - 1);
                                int w = dw;
                                if (widths.containsKey(cidc))
                                    w = widths.get(cidc);
                                metrics.put(new Integer(unic), new int[]{cidc, w});
                            }
                        }
                    }
                    else if (ob.toString().equals("beginbfrange")) {
                        int n = ((PdfNumber)last).intValue();
                        for (int k = 0; k < n; ++k) {
                            String cid1 = decodeString((PdfString)ps.readPRObject());
                            String cid2 = decodeString((PdfString)ps.readPRObject());
                            int cid1c = cid1.charAt(0);
                            int cid2c = cid2.charAt(0);
                            PdfObject ob2 = ps.readPRObject();
                            if (ob2.isString()) {
                                String uni = decodeString((PdfString)ob2);
                                if (uni.length() == 1) {
                                    int unic = uni.charAt(uni.length() - 1);
                                    for (; cid1c <= cid2c; cid1c++, unic++) {
                                        int w = dw;
                                        if (widths.containsKey(cid1c))
                                            w = widths.get(cid1c);
                                        metrics.put(new Integer(unic), new int[]{cid1c, w});
                                    }
                                }
                            }
                            else {
                                ArrayList<PdfObject> ar = ((PdfArray)ob2).getArrayList();
                                for (int j = 0; j < ar.size(); ++j, ++cid1c) {
                                    String uni = decodeString((PdfString)ar.get(j));
                                    if (uni.length() == 1) {
                                        int unic = uni.charAt(uni.length() - 1);
                                        int w = dw;
                                        if (widths.containsKey(cid1c))
                                            w = widths.get(cid1c);
                                        metrics.put(new Integer(unic), new int[]{cid1c, w});
                                    }
                                }
                            }
                        }                        
                    }
                }
                else
                    last = ob;
            }
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
    private void doType1TT() {
        PdfObject enc = PdfReader.getPdfObject(font.get(PdfName.ENCODING));
        if (enc == null)
            fillEncoding(null);
        else {
            if (enc.isName())
                fillEncoding((PdfName)enc);
            else {
                PdfDictionary encDic = (PdfDictionary)enc;
                enc = PdfReader.getPdfObject(encDic.get(PdfName.BASEENCODING));
                if (enc == null)
                    fillEncoding(null);
                else
                    fillEncoding((PdfName)enc);
                PdfArray diffs = (PdfArray)PdfReader.getPdfObject(encDic.get(PdfName.DIFFERENCES));
                if (diffs != null) {
                    ArrayList<PdfObject> dif = diffs.getArrayList();
                    int currentNumber = 0;
                    for (int k = 0; k < dif.size(); ++k) {
                        PdfObject obj = dif.get(k);
                        if (obj.isNumber())
                            currentNumber = ((PdfNumber)obj).intValue();
                        else {
                            int c[] = GlyphList.nameToUnicode(PdfName.decodeName(((PdfName)obj).toString()));
                            if (c != null && c.length > 0) {
                                if (byte2uni.containsKey(currentNumber)) {
                                    uni2byte.remove(byte2uni.get(currentNumber));
                                }
                                uni2byte.put(c[0], currentNumber);
                                byte2uni.put(currentNumber, c[0]);
                            }
                            ++currentNumber;
                        }
                    }
                }
            }
        }
        PdfArray newWidths = (PdfArray)PdfReader.getPdfObject(font.get(PdfName.WIDTHS));
        PdfNumber first = (PdfNumber)PdfReader.getPdfObject(font.get(PdfName.FIRSTCHAR));
        PdfNumber last = (PdfNumber)PdfReader.getPdfObject(font.get(PdfName.LASTCHAR));
        if (BuiltinFonts14.containsKey(fontName)) {
            BaseFont bf;
            try {
                bf = BaseFont.createFont(fontName, WINANSI, false);
            }
            catch (Exception e) {
                throw new ExceptionConverter(e);
            }
            int e[] = uni2byte.toOrderedKeys();
            for (int k = 0; k < e.length; ++k) {
                int n = uni2byte.get(e[k]);
                widths[n] = bf.getRawWidth(n, GlyphList.unicodeToName(e[k]));
            }
            Ascender = bf.getFontDescriptor(ASCENT, 1000);
            CapHeight = bf.getFontDescriptor(CAPHEIGHT, 1000);
            Descender = bf.getFontDescriptor(DESCENT, 1000);
            ItalicAngle = bf.getFontDescriptor(ITALICANGLE, 1000);
            llx = bf.getFontDescriptor(BBOXLLX, 1000);
            lly = bf.getFontDescriptor(BBOXLLY, 1000);
            urx = bf.getFontDescriptor(BBOXURX, 1000);
            ury = bf.getFontDescriptor(BBOXURY, 1000);
        }
        if (first != null && last != null && newWidths != null) {
            int f = first.intValue();
            ArrayList<PdfObject> ar = newWidths.getArrayList();
            for (int k = 0; k < ar.size(); ++k) {
                widths[f + k] = ((PdfNumber)ar.get(k)).intValue();
            }
        }
        fillFontDesc((PdfDictionary)PdfReader.getPdfObject(font.get(PdfName.FONTDESCRIPTOR)));
    }
    
    private void fillFontDesc(PdfDictionary fontDesc) {
        if (fontDesc == null)
            return;
        PdfNumber v = (PdfNumber)PdfReader.getPdfObject(fontDesc.get(PdfName.ASCENT));
        if (v != null)
            Ascender = v.floatValue();
        v = (PdfNumber)PdfReader.getPdfObject(fontDesc.get(PdfName.CAPHEIGHT));
        if (v != null)
            CapHeight = v.floatValue();
        v = (PdfNumber)PdfReader.getPdfObject(fontDesc.get(PdfName.DESCENT));
        if (v != null)
            Descender = v.floatValue();
        v = (PdfNumber)PdfReader.getPdfObject(fontDesc.get(PdfName.ITALICANGLE));
        if (v != null)
            ItalicAngle = v.floatValue();
        PdfArray bbox = (PdfArray)PdfReader.getPdfObject(fontDesc.get(PdfName.FONTBBOX));
        if (bbox != null) {
            ArrayList<PdfObject> ar = bbox.getArrayList();
            llx = ((PdfNumber)ar.get(0)).floatValue();
            lly = ((PdfNumber)ar.get(1)).floatValue();
            urx = ((PdfNumber)ar.get(2)).floatValue();
            ury = ((PdfNumber)ar.get(3)).floatValue();
            if (llx > urx) {
                float t = llx;
                llx = urx;
                urx = t;
            }
            if (lly > ury) {
                float t = lly;
                lly = ury;
                ury = t;
            }
        }
    }
    
    private void fillEncoding(PdfName encoding) {
        if (PdfName.MAC_ROMAN_ENCODING.equals(encoding) || PdfName.WIN_ANSI_ENCODING.equals(encoding)) {
            byte b[] = new byte[256];
            for (int k = 0; k < 256; ++k)
                b[k] = (byte)k;
            String enc = WINANSI;
            if (PdfName.MAC_ROMAN_ENCODING.equals(encoding))
                enc = MACROMAN;
            String cv = PdfEncodings.convertToString(b, enc);
            char arr[] = cv.toCharArray();
            for (int k = 0; k < 256; ++k) {
                uni2byte.put(arr[k], k);
                byte2uni.put(k, arr[k]);
            }
        }
        else {
            for (int k = 0; k < 256; ++k) {
                uni2byte.put(stdEnc[k], k);
                byte2uni.put(k, stdEnc[k]);
            }
        }
    }
    
    
    public String[][] getFamilyFontName() {
        return getFullFontName();
    }
    
    
    public float getFontDescriptor(int key, float fontSize) {
        if (cjkMirror != null)
            return cjkMirror.getFontDescriptor(key, fontSize);
        switch (key) {
            case AWT_ASCENT:
            case ASCENT:
                return Ascender * fontSize / 1000;
            case CAPHEIGHT:
                return CapHeight * fontSize / 1000;
            case AWT_DESCENT:
            case DESCENT:
                return Descender * fontSize / 1000;
            case ITALICANGLE:
                return ItalicAngle;
            case BBOXLLX:
                return llx * fontSize / 1000;
            case BBOXLLY:
                return lly * fontSize / 1000;
            case BBOXURX:
                return urx * fontSize / 1000;
            case BBOXURY:
                return ury * fontSize / 1000;
            case AWT_LEADING:
                return 0;
            case AWT_MAXADVANCE:
                return (urx - llx) * fontSize / 1000;
        }
        return 0;
    }
    
    
    public String[][] getFullFontName() {
        return new String[][]{{"", "", "", fontName}};
    }
    
    
    public String[][] getAllNameEntries() {
        return new String[][]{{"4", "", "", "", fontName}};
    }

    
    public int getKerning(int char1, int char2) {
        return 0;
    }
    
    
    public String getPostscriptFontName() {
        return fontName;
    }
    
    
    int getRawWidth(int c, String name) {
        return 0;
    }
    
    
    public boolean hasKernPairs() {
        return false;
    }
    
    
    void writeFont(PdfWriter writer, PdfIndirectReference ref, Object[] params) throws DocumentException, IOException {
    }

    
    public int getWidth(int char1) {
        if (cjkMirror != null)
            return cjkMirror.getWidth(char1);
        else if (isType0) {
            int[] ws = metrics.get(new Integer(char1));
            if (ws != null)
                return ws[1];
            else
                return 0;
        }
        else
            return super.getWidth(char1);
    }
    
    public int getWidth(String text) {
        if (cjkMirror != null)
            return cjkMirror.getWidth(text);
        else if (isType0) {
            char[] chars = text.toCharArray();
            int len = chars.length;
            int total = 0;
            for (int k = 0; k < len; ++k) {
                int[] ws = metrics.get(new Integer(chars[k]));
                if (ws != null)
                    total += ws[1];
            }
            return total;
        }
        else
            return super.getWidth(text);
    }
    
    byte[] convertToBytes(String text) {
        if (cjkMirror != null)
            return PdfEncodings.convertToBytes(text, CJKFont.CJK_ENCODING);
        else if (isType0) {
            char[] chars = text.toCharArray();
            int len = chars.length;
            byte[] b = new byte[len * 2];
            int bptr = 0;
            for (int k = 0; k < len; ++k) {
                int[] ws = metrics.get(new Integer(chars[k]));
                if (ws != null) {
                    int g = ws[0];
                    b[bptr++] = (byte)(g / 256);
                    b[bptr++] = (byte)(g);
                }
            }
            if (bptr == b.length)
                return b;
            else {
                byte[] nb = new byte[bptr];
                System.arraycopy(b, 0, nb, 0, bptr);
                return nb;
            }
        }
        else {
            char cc[] = text.toCharArray();
            byte b[] = new byte[cc.length];
            int ptr = 0;
            for (int k = 0; k < cc.length; ++k) {
                if (uni2byte.containsKey(cc[k]))
                    b[ptr++] = (byte)uni2byte.get(cc[k]);
            }
            if (ptr == b.length)
                return b;
            else {
                byte[] b2 = new byte[ptr];
                System.arraycopy(b, 0, b2, 0, ptr);
                return b2;
            }
        }
    }
    
    byte[] convertToBytes(int char1) {
        if (cjkMirror != null)
            return PdfEncodings.convertToBytes((char)char1, CJKFont.CJK_ENCODING);
        else if (isType0) {
            int[] ws = metrics.get(new Integer(char1));
            if (ws != null) {
                int g = ws[0];
                return new byte[]{(byte)(g / 256), (byte)(g)};
            }
            else
                return new byte[0];
        }
        else {
            if (uni2byte.containsKey(char1))
                return new byte[]{(byte)uni2byte.get(char1)};
            else
                return new byte[0];
        }
    }
    
    PdfIndirectReference getIndirectReference() {
        return refFont;
    }
    
    public boolean charExists(int c) {
        if (cjkMirror != null)
            return cjkMirror.charExists(c);
        else if (isType0) {
            return metrics.containsKey(new Integer(c));
        }
        else
            return super.charExists(c);
    }
    
        
    public void setPostscriptFontName(String name) {
    }
    
    public boolean setKerning(int char1, int char2, int kern) {
        return false;
    }
    
    public int[] getCharBBox(int c) {
        return null;
    }
    
    protected int[] getRawCharBBox(int c, String name) {
        return null;
    }
}