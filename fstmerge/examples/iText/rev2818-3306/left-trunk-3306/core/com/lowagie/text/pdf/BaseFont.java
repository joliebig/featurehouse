

package com.lowagie.text.pdf;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.StringTokenizer;

import com.lowagie.text.DocumentException;



public abstract class BaseFont {
    
    
    public static final String COURIER = "Courier";
    
    
    public static final String COURIER_BOLD = "Courier-Bold";
    
    
    public static final String COURIER_OBLIQUE = "Courier-Oblique";
    
    
    public static final String COURIER_BOLDOBLIQUE = "Courier-BoldOblique";
    
    
    public static final String HELVETICA = "Helvetica";
    
    
    public static final String HELVETICA_BOLD = "Helvetica-Bold";
    
    
    public static final String HELVETICA_OBLIQUE = "Helvetica-Oblique";
    
    
    public static final String HELVETICA_BOLDOBLIQUE = "Helvetica-BoldOblique";
    
    
    public static final String SYMBOL = "Symbol";
    
    
    public static final String TIMES_ROMAN = "Times-Roman";
    
    
    public static final String TIMES_BOLD = "Times-Bold";
    
    
    public static final String TIMES_ITALIC = "Times-Italic";
    
    
    public static final String TIMES_BOLDITALIC = "Times-BoldItalic";
    
    
    public static final String ZAPFDINGBATS = "ZapfDingbats";
    
        
    public static final int ASCENT = 1;    
        
    public static final int CAPHEIGHT = 2;
        
    public static final int DESCENT = 3;
        
    public static final int ITALICANGLE = 4;
        
    public static final int BBOXLLX = 5;
        
    public static final int BBOXLLY = 6;
        
    public static final int BBOXURX = 7;
        
    public static final int BBOXURY = 8;
    
    
    public static final int AWT_ASCENT = 9;
    
    public static final int AWT_DESCENT = 10;
    
    public static final int AWT_LEADING = 11;
    
    public static final int AWT_MAXADVANCE = 12;    
    
    public static final int UNDERLINE_POSITION = 13;
    
    public static final int UNDERLINE_THICKNESS = 14;
    
    public static final int STRIKETHROUGH_POSITION = 15;
    
    public static final int STRIKETHROUGH_THICKNESS = 16;
    
    public static final int SUBSCRIPT_SIZE = 17;
    
    public static final int SUBSCRIPT_OFFSET = 18;
    
    public static final int SUPERSCRIPT_SIZE = 19;
    
    public static final int SUPERSCRIPT_OFFSET = 20;
        
    public static final int FONT_TYPE_T1 = 0;
        
    public static final int FONT_TYPE_TT = 1;
        
    public static final int FONT_TYPE_CJK = 2;
        
    public static final int FONT_TYPE_TTUNI = 3;
        
    public static final int FONT_TYPE_DOCUMENT = 4;
        
    public static final int FONT_TYPE_T3 = 5;
        
    public static final String IDENTITY_H = "Identity-H";
        
    public static final String IDENTITY_V = "Identity-V";
    
        
    public static final String CP1250 = "Cp1250";
    
        
    public static final String CP1252 = "Cp1252";
    
        
    public static final String CP1257 = "Cp1257";
    
        
    public static final String WINANSI = "Cp1252";
    
        
    public static final String MACROMAN = "MacRoman";
    
    public static final int[] CHAR_RANGE_LATIN = {0, 0x17f, 0x2000, 0x206f, 0x20a0, 0x20cf, 0xfb00, 0xfb06};
    public static final int[] CHAR_RANGE_ARABIC = {0, 0x7f, 0x0600, 0x067f, 0x20a0, 0x20cf, 0xfb50, 0xfbff, 0xfe70, 0xfeff};
    public static final int[] CHAR_RANGE_HEBREW = {0, 0x7f, 0x0590, 0x05ff, 0x20a0, 0x20cf, 0xfb1d, 0xfb4f};
    public static final int[] CHAR_RANGE_CYRILLIC = {0, 0x7f, 0x0400, 0x052f, 0x2000, 0x206f, 0x20a0, 0x20cf};


    public static final boolean EMBEDDED = true;
    

    public static final boolean NOT_EMBEDDED = false;

    public static final boolean CACHED = true;

    public static final boolean NOT_CACHED = false;
    
        
    public static final String RESOURCE_PATH = "com/lowagie/text/pdf/fonts/";
        
    public static final char CID_NEWLINE = '\u';
    
    protected ArrayList subsetRanges;
        
    int fontType;

    public static final String notdef = ".notdef";
    

    protected int widths[] = new int[256];
    

    protected String differences[] = new String[256];

    protected char unicodeDifferences[] = new char[256];
    
    protected int charBBoxes[][] = new int[256][];

    protected String encoding;
    

    protected boolean embedded;
    

    protected boolean fontSpecific = true;
    

    protected static HashMap fontCache = new HashMap();
    

    protected static final HashMap BuiltinFonts14 = new HashMap();
    
    
    protected boolean forceWidthsOutput = false;
    
    
    protected boolean directTextToByte = false;
    
    
    protected boolean subset = true;
    
    protected boolean fastWinansi = false;
    
    
    protected IntHashtable specialMap;
    
    static {
        BuiltinFonts14.put(COURIER, PdfName.COURIER);
        BuiltinFonts14.put(COURIER_BOLD, PdfName.COURIER_BOLD);
        BuiltinFonts14.put(COURIER_BOLDOBLIQUE, PdfName.COURIER_BOLDOBLIQUE);
        BuiltinFonts14.put(COURIER_OBLIQUE, PdfName.COURIER_OBLIQUE);
        BuiltinFonts14.put(HELVETICA, PdfName.HELVETICA);
        BuiltinFonts14.put(HELVETICA_BOLD, PdfName.HELVETICA_BOLD);
        BuiltinFonts14.put(HELVETICA_BOLDOBLIQUE, PdfName.HELVETICA_BOLDOBLIQUE);
        BuiltinFonts14.put(HELVETICA_OBLIQUE, PdfName.HELVETICA_OBLIQUE);
        BuiltinFonts14.put(SYMBOL, PdfName.SYMBOL);
        BuiltinFonts14.put(TIMES_ROMAN, PdfName.TIMES_ROMAN);
        BuiltinFonts14.put(TIMES_BOLD, PdfName.TIMES_BOLD);
        BuiltinFonts14.put(TIMES_BOLDITALIC, PdfName.TIMES_BOLDITALIC);
        BuiltinFonts14.put(TIMES_ITALIC, PdfName.TIMES_ITALIC);
        BuiltinFonts14.put(ZAPFDINGBATS, PdfName.ZAPFDINGBATS);
    }
    
    
    static class StreamFont extends PdfStream {
        
        
        public StreamFont(byte contents[], int lengths[]) throws DocumentException {
            try {
                bytes = contents;
                put(PdfName.LENGTH, new PdfNumber(bytes.length));
                for (int k = 0; k < lengths.length; ++k) {
                    put(new PdfName("Length" + (k + 1)), new PdfNumber(lengths[k]));
                }
                flateCompress();
            }
            catch (Exception e) {
                throw new DocumentException(e);
            }
        }
        
        
        public StreamFont(byte contents[], String subType) throws DocumentException {
            try {
                bytes = contents;
                put(PdfName.LENGTH, new PdfNumber(bytes.length));
                if (subType != null)
                    put(PdfName.SUBTYPE, new PdfName(subType));
                flateCompress();
            }
            catch (Exception e) {
                throw new DocumentException(e);
            }
        }
    }
    
    
    protected BaseFont() {
    }
    
    
    public BaseFont createFont() throws DocumentException, IOException {
        return createFont(BaseFont.HELVETICA, BaseFont.WINANSI, BaseFont.NOT_EMBEDDED);
    }
    
    
    public static BaseFont createFont(String name, String encoding, boolean embedded) throws DocumentException, IOException {
        return createFont(name, encoding, embedded, true, null, null);
    }
    
    
    public static BaseFont createFont(String name, String encoding, boolean embedded, boolean cached, byte ttfAfm[], byte pfb[]) throws DocumentException, IOException {
        return createFont(name, encoding, embedded, cached, ttfAfm, pfb, false);
    }
    
    
    public static BaseFont createFont(String name, String encoding, boolean embedded, boolean cached, byte ttfAfm[], byte pfb[], boolean noThrow) throws DocumentException, IOException {
        String nameBase = getBaseName(name);
        encoding = normalizeEncoding(encoding);
        boolean isBuiltinFonts14 = BuiltinFonts14.containsKey(name);
        boolean isCJKFont = isBuiltinFonts14 ? false : CJKFont.isCJKFont(nameBase, encoding);
        if (isBuiltinFonts14 || isCJKFont)
            embedded = false;
        else if (encoding.equals(IDENTITY_H) || encoding.equals(IDENTITY_V))
            embedded = true;
        BaseFont fontFound = null;
        BaseFont fontBuilt = null;
        String key = name + "\n" + encoding + "\n" + embedded;
        if (cached) {
            synchronized (fontCache) {
                fontFound = (BaseFont)fontCache.get(key);
            }
            if (fontFound != null)
                return fontFound;
        }
        if (isBuiltinFonts14 || name.toLowerCase().endsWith(".afm") || name.toLowerCase().endsWith(".pfm")) {
            fontBuilt = new Type1Font(name, encoding, embedded, ttfAfm, pfb);
            fontBuilt.fastWinansi = encoding.equals(CP1252);
        }
        else if (nameBase.toLowerCase().endsWith(".ttf") || nameBase.toLowerCase().endsWith(".otf") || nameBase.toLowerCase().indexOf(".ttc,") > 0) {
            if (encoding.equals(IDENTITY_H) || encoding.equals(IDENTITY_V))
                fontBuilt = new TrueTypeFontUnicode(name, encoding, embedded, ttfAfm);
            else {
                fontBuilt = new TrueTypeFont(name, encoding, embedded, ttfAfm);
                fontBuilt.fastWinansi = encoding.equals(CP1252);
            }
        }
        else if (isCJKFont)
            fontBuilt = new CJKFont(name, encoding, embedded);
        else if (noThrow)
            return null;
        else
            throw new DocumentException("Font '" + name + "' with '" + encoding + "' is not recognized.");
        if (cached) {
            synchronized (fontCache) {
                fontFound = (BaseFont)fontCache.get(key);
                if (fontFound != null)
                    return fontFound;
                fontCache.put(key, fontBuilt);
            }
        }
        return fontBuilt;
    }
    
        
    public static BaseFont createFont(PRIndirectReference fontRef) {
        return new DocumentFont(fontRef);
    }
    
    
    protected static String getBaseName(String name) {
        if (name.endsWith(",Bold"))
            return name.substring(0, name.length() - 5);
        else if (name.endsWith(",Italic"))
            return name.substring(0, name.length() - 7);
        else if (name.endsWith(",BoldItalic"))
            return name.substring(0, name.length() - 11);
        else
            return name;
    }
    
    
    protected static String normalizeEncoding(String enc) {
        if (enc.equals("winansi") || enc.equals(""))
            return CP1252;
        else if (enc.equals("macroman"))
            return MACROMAN;
        else
            return enc;
    }
    
    
    protected void createEncoding() {
        if (encoding.startsWith("#")) {
            specialMap = new IntHashtable();
            StringTokenizer tok = new StringTokenizer(encoding.substring(1), " ,\t\n\r\f");
            if (tok.nextToken().equals("full")) {
                while (tok.hasMoreTokens()) {
                    String order = tok.nextToken();
                    String name = tok.nextToken();
                    char uni = (char)Integer.parseInt(tok.nextToken(), 16);
                    int orderK;
                    if (order.startsWith("'"))
                        orderK = (int)order.charAt(1);
                    else
                        orderK = Integer.parseInt(order);
                    orderK %= 256;
                    specialMap.put((int)uni, orderK);
                    differences[orderK] = name;
                    unicodeDifferences[orderK] = uni;
                    widths[orderK] = getRawWidth((int)uni, name);
                    charBBoxes[orderK] = getRawCharBBox((int)uni, name);
                }
            }
            else {
                int k = 0;
                if (tok.hasMoreTokens())
                    k = Integer.parseInt(tok.nextToken());
                while (tok.hasMoreTokens() && k < 256) {
                    String hex = tok.nextToken();
                    int uni = Integer.parseInt(hex, 16) % 0x10000;
                    String name = GlyphList.unicodeToName(uni);
                    if (name != null) {
                        specialMap.put(uni, k);
                        differences[k] = name;
                        unicodeDifferences[k] = (char)uni;
                        widths[k] = getRawWidth(uni, name);
                        charBBoxes[k] = getRawCharBBox(uni, name);
                        ++k;
                    }
                }
            }
            for (int k = 0; k < 256; ++k) {
                if (differences[k] == null) {
                    differences[k] = notdef;
                }
            }
        }
        else if (fontSpecific) {
            for (int k = 0; k < 256; ++k) {
                widths[k] = getRawWidth(k, null);
                charBBoxes[k] = getRawCharBBox(k, null);
            }
        }
        else {
            String s;
            String name;
            char c;
            byte b[] = new byte[1];
            for (int k = 0; k < 256; ++k) {
                b[0] = (byte)k;
                s = PdfEncodings.convertToString(b, encoding);
                if (s.length() > 0) {
                    c = s.charAt(0);
                }
                else {
                    c = '?';
                }
                name = GlyphList.unicodeToName((int)c);
                if (name == null)
                    name = notdef;
                differences[k] = name;
                unicodeDifferences[k] = c;
                widths[k] = getRawWidth((int)c, name);
                charBBoxes[k] = getRawCharBBox((int)c, name);
            }
        }
    }
    
    
    abstract int getRawWidth(int c, String name);
    
    
    public abstract int getKerning(char char1, char char2);

    
    public abstract boolean setKerning(char char1, char char2, int kern);
    
    
    public int getWidth(char char1) {
        if (fastWinansi) {
            if (char1 < 128 || (char1 >= 160 && char1 <= 255))
                return widths[char1];
            else
                return widths[PdfEncodings.winansi.get(char1)];
        }
        else {
            int total = 0;
            byte mbytes[] = convertToBytes(char1);
            for (int k = 0; k < mbytes.length; ++k)
                total += widths[0xff & mbytes[k]];
            return total;
        }
    }
    
    
    public int getWidth(String text) {
        int total = 0;
        if (fastWinansi) {
            int len = text.length();
            for (int k = 0; k < len; ++k) {
                char char1 = text.charAt(k);
                if (char1 < 128 || (char1 >= 160 && char1 <= 255))
                    total += widths[char1];
                else
                    total += widths[PdfEncodings.winansi.get(char1)];
            }
            return total;
        }
        else {
            byte mbytes[] = convertToBytes(text);
            for (int k = 0; k < mbytes.length; ++k)
                total += widths[0xff & mbytes[k]];
        }
        return total;
    }
    

    public int getDescent(String text) {
        int min = 0;
        char chars[] = text.toCharArray();
        for (int k = 0; k < chars.length; ++k) {
            int bbox[] = getCharBBox(chars[k]);
            if (bbox != null && bbox[1] < min)
                min = bbox[1];
        }
        return min;
    }
    

    public int getAscent(String text) {
        int max = 0;
        char chars[] = text.toCharArray();
        for (int k = 0; k < chars.length; ++k) {
            int bbox[] = getCharBBox(chars[k]);
            if (bbox != null && bbox[3] > max)
                max = bbox[3];
        }
        return max;
    }


    public float getDescentPoint(String text, float fontSize)
    {
        return (float)getDescent(text) * 0.001f * fontSize;
    }
    

    public float getAscentPoint(String text, float fontSize)
    {
        return (float)getAscent(text) * 0.001f * fontSize;
    }

    
    
    public float getWidthPointKerned(String text, float fontSize) {
        float size = (float)getWidth(text) * 0.001f * fontSize;
        if (!hasKernPairs())
            return size;
        int len = text.length() - 1;
        int kern = 0;
        char c[] = text.toCharArray();
        for (int k = 0; k < len; ++k) {
            kern += getKerning(c[k], c[k + 1]);
        }
        return size + kern * 0.001f * fontSize;
    }
    
    
    public float getWidthPoint(String text, float fontSize) {
        return (float)getWidth(text) * 0.001f * fontSize;
    }
    
    
    public float getWidthPoint(char char1, float fontSize) {
        return getWidth(char1) * 0.001f * fontSize;
    }
    
    
    byte[] convertToBytes(String text) {
        if (directTextToByte)
            return PdfEncodings.convertToBytes(text, null);
        if (specialMap != null) {
            byte[] b = new byte[text.length()];
            int ptr = 0;
            int length = text.length();
            for (int k = 0; k < length; ++k) {
                char c = text.charAt(k);
                if (specialMap.containsKey((int)c))
                    b[ptr++] = (byte)specialMap.get((int)c);
            }
            if (ptr < length) {
                byte[] b2 = new byte[ptr];
                System.arraycopy(b, 0, b2, 0, ptr);
                return b2;
            }
            else
                return b;
        }
        return PdfEncodings.convertToBytes(text, encoding);
    }
    
    
    byte[] convertToBytes(char char1) {
        if (directTextToByte)
            return PdfEncodings.convertToBytes(char1, null);
        if (specialMap != null) {
            if (specialMap.containsKey((int)char1))
                return new byte[]{(byte)specialMap.get((int)char1)};
            else
                return new byte[0];
        }
        return PdfEncodings.convertToBytes(char1, encoding);
    }
    
    
    abstract void writeFont(PdfWriter writer, PdfIndirectReference ref, Object params[]) throws DocumentException, IOException;
    
    
    public String getEncoding() {
        return encoding;
    }
    
    
    public abstract float getFontDescriptor(int key, float fontSize);
    
    
    public int getFontType() {
        return fontType;
    }
    
    
    public boolean isEmbedded() {
        return embedded;
    }
    
    
    public boolean isFontSpecific() {
        return fontSpecific;
    }
    
    
    public static String createSubsetPrefix() {
        String s = "";
        for (int k = 0; k < 6; ++k)
            s += (char)(Math.random() * 26 + 'A');
        return s + "+";
    }
    
    
    char getUnicodeDifferences(int index) {
        return unicodeDifferences[index];
    }
    
    
    public abstract String getPostscriptFontName();
    
        
    public abstract void setPostscriptFontName(String name);
    
    
    public abstract String[][] getFullFontName();
    
    
    public abstract String[][] getAllNameEntries(); 

        
    public static String[][] getFullFontName(String name, String encoding, byte ttfAfm[]) throws DocumentException, IOException {
        String nameBase = getBaseName(name);
        BaseFont fontBuilt = null;
        if (nameBase.toLowerCase().endsWith(".ttf") || nameBase.toLowerCase().endsWith(".otf") || nameBase.toLowerCase().indexOf(".ttc,") > 0)
            fontBuilt = new TrueTypeFont(name, CP1252, false, ttfAfm, true);
        else
            fontBuilt = createFont(name, encoding, false, false, ttfAfm, null);
        return fontBuilt.getFullFontName();
    }
    
        
    public static Object[] getAllFontNames(String name, String encoding, byte ttfAfm[]) throws DocumentException, IOException {
        String nameBase = getBaseName(name);
        BaseFont fontBuilt = null;
        if (nameBase.toLowerCase().endsWith(".ttf") || nameBase.toLowerCase().endsWith(".otf") || nameBase.toLowerCase().indexOf(".ttc,") > 0)
            fontBuilt = new TrueTypeFont(name, CP1252, false, ttfAfm, true);
        else
            fontBuilt = createFont(name, encoding, false, false, ttfAfm, null);
        return new Object[]{fontBuilt.getPostscriptFontName(), fontBuilt.getFamilyFontName(), fontBuilt.getFullFontName()};
    }
    
    
    public static String[][] getAllNameEntries(String name, String encoding, byte ttfAfm[]) throws DocumentException, IOException {
        String nameBase = getBaseName(name);
        BaseFont fontBuilt = null;
        if (nameBase.toLowerCase().endsWith(".ttf") || nameBase.toLowerCase().endsWith(".otf") || nameBase.toLowerCase().indexOf(".ttc,") > 0)
            fontBuilt = new TrueTypeFont(name, CP1252, false, ttfAfm, true);
        else
            fontBuilt = createFont(name, encoding, false, false, ttfAfm, null);
        return fontBuilt.getAllNameEntries();
    }
    
    
    public abstract String[][] getFamilyFontName();
    
    
    public String[] getCodePagesSupported() {
        return new String[0];
    }
    
        
    public static String[] enumerateTTCNames(String ttcFile) throws DocumentException, IOException {
        return new EnumerateTTC(ttcFile).getNames();
    }

        
    public static String[] enumerateTTCNames(byte ttcArray[]) throws DocumentException, IOException {
        return new EnumerateTTC(ttcArray).getNames();
    }
    
        
    public int[] getWidths() {
        return widths;
    }

        
    public String[] getDifferences() {
        return differences;
    }

        
    public char[] getUnicodeDifferences() {
        return unicodeDifferences;
    }
    
    
    public boolean isForceWidthsOutput() {
        return forceWidthsOutput;
    }
    
    
    public void setForceWidthsOutput(boolean forceWidthsOutput) {
        this.forceWidthsOutput = forceWidthsOutput;
    }
    
    
    public boolean isDirectTextToByte() {
        return directTextToByte;
    }
    
    
    public void setDirectTextToByte(boolean directTextToByte) {
        this.directTextToByte = directTextToByte;
    }
    
    
    public boolean isSubset() {
        return subset;
    }
    
    
    public void setSubset(boolean subset) {
        this.subset = subset;
    }

        
    public static InputStream getResourceStream(String key) {
        return getResourceStream(key, null);
    }
    
        
    public static InputStream getResourceStream(String key, ClassLoader loader) {
        if (key.startsWith("/"))
            key = key.substring(1);
        InputStream is = null;
        if (loader != null) {
            is = loader.getResourceAsStream(key);
            if (is != null)
                return is;
        }
        
        try {
            ClassLoader contextClassLoader = Thread.currentThread().getContextClassLoader();
            if (contextClassLoader != null) {
                is = contextClassLoader.getResourceAsStream(key);
            }
        } catch (Throwable e) {}

        if (is == null) {
            is = BaseFont.class.getResourceAsStream("/" + key);
        }
        if (is == null) {
            is = ClassLoader.getSystemResourceAsStream(key);
        }
        return is;
    }
    
        
    public char getUnicodeEquivalent(char c) {
        return c;
    }
    
        
    public char getCidCode(char c) {
        return c;
    }

        
    public abstract boolean hasKernPairs();
    
        
    public boolean charExists(char c) {
        byte b[] = convertToBytes(c);
        return b.length > 0;
    }
    
        
    public boolean setCharAdvance(char c, int advance) {
        byte b[] = convertToBytes(c);
        if (b.length == 0)
            return false;
        widths[0xff & b[0]] = advance;
        return true;
    }
    
    private static void addFont(PRIndirectReference fontRef, IntHashtable hits, ArrayList fonts) {
        PdfObject obj = PdfReader.getPdfObject(fontRef);
        if (obj == null || !obj.isDictionary())
            return;
        PdfDictionary font = (PdfDictionary)obj;
        PdfName subtype = (PdfName)PdfReader.getPdfObject(font.get(PdfName.SUBTYPE));
        if (!PdfName.TYPE1.equals(subtype) && !PdfName.TRUETYPE.equals(subtype))
            return;
        PdfName name = (PdfName)PdfReader.getPdfObject(font.get(PdfName.BASEFONT));
        fonts.add(new Object[]{PdfName.decodeName(name.toString()), fontRef});
        hits.put(fontRef.getNumber(), 1);
    }
    
    private static void recourseFonts(PdfDictionary page, IntHashtable hits, ArrayList fonts, int level) {
        ++level;
        if (level > 50) 
            return;
        PdfDictionary resources = (PdfDictionary)PdfReader.getPdfObject(page.get(PdfName.RESOURCES));
        if (resources == null)
            return;
        PdfDictionary font = (PdfDictionary)PdfReader.getPdfObject(resources.get(PdfName.FONT));
        if (font != null) {
            for (Iterator it = font.getKeys().iterator(); it.hasNext();) {
                PdfObject ft = font.get((PdfName)it.next());        
                if (ft == null || !ft.isIndirect())
                    continue;
                int hit = ((PRIndirectReference)ft).getNumber();
                if (hits.containsKey(hit))
                    continue;
                addFont((PRIndirectReference)ft, hits, fonts);
            }
        }
        PdfDictionary xobj = (PdfDictionary)PdfReader.getPdfObject(resources.get(PdfName.XOBJECT));
        if (xobj != null) {
            for (Iterator it = xobj.getKeys().iterator(); it.hasNext();) {
                recourseFonts((PdfDictionary)PdfReader.getPdfObject(xobj.get((PdfName)it.next())), hits, fonts, level);
            }
        }
    }
    
        
    public static ArrayList getDocumentFonts(PdfReader reader) {
        IntHashtable hits = new IntHashtable();
        ArrayList fonts = new ArrayList();
        int npages = reader.getNumberOfPages();
        for (int k = 1; k <= npages; ++k)
            recourseFonts(reader.getPageN(k), hits, fonts, 1);
        return fonts;
    }
    
        
    public static ArrayList getDocumentFonts(PdfReader reader, int page) {
        IntHashtable hits = new IntHashtable();
        ArrayList fonts = new ArrayList();
        recourseFonts(reader.getPageN(page), hits, fonts, 1);
        return fonts;
    }
    
        
    public int[] getCharBBox(char c) {
        byte b[] = convertToBytes(c);
        if (b.length == 0)
            return null;
        else
            return charBBoxes[b[0] & 0xff];
    }
    
    protected abstract int[] getRawCharBBox(int c, String name);

        
    public void correctArabicAdvance() {
        for (char c = '\u'; c <= '\u'; ++c)
            setCharAdvance(c, 0);
        setCharAdvance('\u', 0);
        for (char c = '\u'; c <= '\u'; ++c)
            setCharAdvance(c, 0);
        for (char c = '\u'; c <= '\u'; ++c)
            setCharAdvance(c, 0);
        for (char c = '\u'; c <= '\u'; ++c)
            setCharAdvance(c, 0);
        for (char c = '\u'; c <= '\u'; ++c)
            setCharAdvance(c, 0);
    }
    
    
    public void addSubsetRange(int[] range) {
        if (subsetRanges == null)
            subsetRanges = new ArrayList();
        subsetRanges.add(range);
    }
}
