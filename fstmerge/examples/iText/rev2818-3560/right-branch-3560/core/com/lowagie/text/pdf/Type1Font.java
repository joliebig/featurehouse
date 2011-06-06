

package com.lowagie.text.pdf;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.StringTokenizer;

import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.fonts.FontsResourceAnchor;


class Type1Font extends BaseFont
{
    private static FontsResourceAnchor resourceAnchor;
    
        
    protected byte pfb[];

    private String FontName;

    private String FullName;

    private String FamilyName;

    private String Weight = "";

    private float ItalicAngle = 0.0f;

    private boolean IsFixedPitch = false;

    private String CharacterSet;

    private int llx = -50;

    private int lly = -200;

    private int urx = 1000;

    private int ury = 900;

    private int UnderlinePosition = -100;

    private int UnderlineThickness = 50;

    private String EncodingScheme = "FontSpecific";

    private int CapHeight = 700;

    private int XHeight = 480;

    private int Ascender = 800;

    private int Descender = -200;

    private int StdHW;

    private int StdVW = 80;
    

    private HashMap<Object, Object[]> CharMetrics = new HashMap<Object, Object[]>();

    private HashMap<String, Object[]> KernPairs = new HashMap<String, Object[]>();

    private String fileName;

    private boolean builtinFont = false;

    private static final int PFB_TYPES[] = {1, 2, 1};
    
    
    Type1Font(String afmFile, String enc, boolean emb, byte ttfAfm[], byte pfb[]) throws DocumentException, IOException
    {
        if (emb && ttfAfm != null && pfb == null)
            throw new DocumentException("Two byte arrays are needed if the Type1 font is embedded.");
        if (emb && ttfAfm != null)
            this.pfb = pfb;
        encoding = enc;
        embedded = emb;
        fileName = afmFile;
        fontType = FONT_TYPE_T1;
        RandomAccessFileOrArray rf = null;
        InputStream is = null;
        if (BuiltinFonts14.containsKey(afmFile)) {
            embedded = false;
            builtinFont = true;
            byte buf[] = new byte[1024];
            try {
                if (resourceAnchor == null)
                    resourceAnchor = new FontsResourceAnchor();
                is = getResourceStream(RESOURCE_PATH + afmFile + ".afm", resourceAnchor.getClass().getClassLoader());
                if (is == null) {
                    String msg = afmFile + " not found as resource. (The *.afm files must exist as resources in the package com.lowagie.text.pdf.fonts)";
                    System.err.println(msg);
                    throw new DocumentException(msg);
                }
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                while (true) {
                    int size = is.read(buf);
                    if (size < 0)
                        break;
                    out.write(buf, 0, size);
                }
                buf = out.toByteArray();
            }
            finally {
                if (is != null) {
                    try {
                        is.close();
                    }
                    catch (Exception e) {
                        
                    }
                }
            }
            try {
                rf = new RandomAccessFileOrArray(buf);
                process(rf);
            }
            finally {
                if (rf != null) {
                    try {
                        rf.close();
                    }
                    catch (Exception e) {
                        
                    }
                }
            }
        }
        else if (afmFile.toLowerCase().endsWith(".afm")) {
            try {
                if (ttfAfm == null)
                    rf = new RandomAccessFileOrArray(afmFile);
                else
                    rf = new RandomAccessFileOrArray(ttfAfm);
                process(rf);
            }
            finally {
                if (rf != null) {
                    try {
                        rf.close();
                    }
                    catch (Exception e) {
                        
                    }
                }
            }
        }
        else if (afmFile.toLowerCase().endsWith(".pfm")) {
            try {
                ByteArrayOutputStream ba = new ByteArrayOutputStream();
                if (ttfAfm == null)
                    rf = new RandomAccessFileOrArray(afmFile);
                else
                    rf = new RandomAccessFileOrArray(ttfAfm);
                Pfm2afm.convert(rf, ba);
                rf.close();
                rf = new RandomAccessFileOrArray(ba.toByteArray());
                process(rf);
            }
            finally {
                if (rf != null) {
                    try {
                        rf.close();
                    }
                    catch (Exception e) {
                        
                    }
                }
            }
        }
        else
            throw new DocumentException(afmFile + " is not an AFM or PFM font file.");

        EncodingScheme = EncodingScheme.trim();
        if (EncodingScheme.equals("AdobeStandardEncoding") || EncodingScheme.equals("StandardEncoding")) {
            fontSpecific = false;
        }
        if (!encoding.startsWith("#"))
            PdfEncodings.convertToBytes(" ", enc); 
        createEncoding();
    }
    

    int getRawWidth(int c, String name) {
        Object metrics[];
        if (name == null) { 
            metrics = CharMetrics.get(new Integer(c));
        }
        else {
            if (name.equals(".notdef"))
                return 0;
            metrics = CharMetrics.get(name);
        }
        if (metrics != null)
            return ((Integer)(metrics[1])).intValue();
        return 0;
    }
    

    public int getKerning(int char1, int char2)
    {
        String first = GlyphList.unicodeToName(char1);
        if (first == null)
            return 0;
        String second = GlyphList.unicodeToName(char2);
        if (second == null)
            return 0;
        Object obj[] = KernPairs.get(first);
        if (obj == null)
            return 0;
        for (int k = 0; k < obj.length; k += 2) {
            if (second.equals(obj[k]))
                return ((Integer)obj[k + 1]).intValue();
        }
        return 0;
    }
    
    
    
    public void process(RandomAccessFileOrArray rf) throws DocumentException, IOException
    {
        String line;
        boolean isMetrics = false;
        while ((line = rf.readLine()) != null)
        {
            StringTokenizer tok = new StringTokenizer(line, " ,\n\r\t\f");
            if (!tok.hasMoreTokens())
                continue;
            String ident = tok.nextToken();
            if (ident.equals("FontName"))
                FontName = tok.nextToken("\u").substring(1);
            else if (ident.equals("FullName"))
                FullName = tok.nextToken("\u").substring(1);
            else if (ident.equals("FamilyName"))
                FamilyName = tok.nextToken("\u").substring(1);
            else if (ident.equals("Weight"))
                Weight = tok.nextToken("\u").substring(1);
            else if (ident.equals("ItalicAngle"))
                ItalicAngle = Float.parseFloat(tok.nextToken());
            else if (ident.equals("IsFixedPitch"))
                IsFixedPitch = tok.nextToken().equals("true");
            else if (ident.equals("CharacterSet"))
                CharacterSet = tok.nextToken("\u").substring(1);
            else if (ident.equals("FontBBox"))
            {
                llx = (int)Float.parseFloat(tok.nextToken());
                lly = (int)Float.parseFloat(tok.nextToken());
                urx = (int)Float.parseFloat(tok.nextToken());
                ury = (int)Float.parseFloat(tok.nextToken());
            }
            else if (ident.equals("UnderlinePosition"))
                UnderlinePosition = (int)Float.parseFloat(tok.nextToken());
            else if (ident.equals("UnderlineThickness"))
                UnderlineThickness = (int)Float.parseFloat(tok.nextToken());
            else if (ident.equals("EncodingScheme"))
                EncodingScheme = tok.nextToken("\u").substring(1);
            else if (ident.equals("CapHeight"))
                CapHeight = (int)Float.parseFloat(tok.nextToken());
            else if (ident.equals("XHeight"))
                XHeight = (int)Float.parseFloat(tok.nextToken());
            else if (ident.equals("Ascender"))
                Ascender = (int)Float.parseFloat(tok.nextToken());
            else if (ident.equals("Descender"))
                Descender = (int)Float.parseFloat(tok.nextToken());
            else if (ident.equals("StdHW"))
                StdHW = (int)Float.parseFloat(tok.nextToken());
            else if (ident.equals("StdVW"))
                StdVW = (int)Float.parseFloat(tok.nextToken());
            else if (ident.equals("StartCharMetrics"))
            {
                isMetrics = true;
                break;
            }
        }
        if (!isMetrics)
            throw new DocumentException("Missing StartCharMetrics in " + fileName);
        while ((line = rf.readLine()) != null)
        {
            StringTokenizer tok = new StringTokenizer(line);
            if (!tok.hasMoreTokens())
                continue;
            String ident = tok.nextToken();
            if (ident.equals("EndCharMetrics"))
            {
                isMetrics = false;
                break;
            }
            Integer C = new Integer(-1);
            Integer WX = new Integer(250);
            String N = "";
            int B[] = null;

            tok = new StringTokenizer(line, ";");
            while (tok.hasMoreTokens())
            {
                StringTokenizer tokc = new StringTokenizer(tok.nextToken());
                if (!tokc.hasMoreTokens())
                    continue;
                ident = tokc.nextToken();
                if (ident.equals("C"))
                    C = Integer.valueOf(tokc.nextToken());
                else if (ident.equals("WX"))
                    WX = new Integer((int)Float.parseFloat(tokc.nextToken()));
                else if (ident.equals("N"))
                    N = tokc.nextToken();
                else if (ident.equals("B")) {
                    B = new int[]{Integer.parseInt(tokc.nextToken()), 
                                         Integer.parseInt(tokc.nextToken()),
                                         Integer.parseInt(tokc.nextToken()),
                                         Integer.parseInt(tokc.nextToken())};
                }
            }
            Object metrics[] = new Object[]{C, WX, N, B};
            if (C.intValue() >= 0)
                CharMetrics.put(C, metrics);
            CharMetrics.put(N, metrics);
        }
        if (isMetrics)
            throw new DocumentException("Missing EndCharMetrics in " + fileName);
        if (!CharMetrics.containsKey("nonbreakingspace")) {
            Object[] space = CharMetrics.get("space");
            if (space != null)
                CharMetrics.put("nonbreakingspace", space);
        }
        while ((line = rf.readLine()) != null)
        {
            StringTokenizer tok = new StringTokenizer(line);
            if (!tok.hasMoreTokens())
                continue;
            String ident = tok.nextToken();
            if (ident.equals("EndFontMetrics"))
                return;
            if (ident.equals("StartKernPairs"))
            {
                isMetrics = true;
                break;
            }
        }
        if (!isMetrics)
            throw new DocumentException("Missing EndFontMetrics in " + fileName);
        while ((line = rf.readLine()) != null)
        {
            StringTokenizer tok = new StringTokenizer(line);
            if (!tok.hasMoreTokens())
                continue;
            String ident = tok.nextToken();
            if (ident.equals("KPX"))
            {
                String first = tok.nextToken();
                String second = tok.nextToken();
                Integer width = new Integer((int)Float.parseFloat(tok.nextToken()));
                Object relates[] = KernPairs.get(first);
                if (relates == null)
                    KernPairs.put(first, new Object[]{second, width});
                else
                {
                    int n = relates.length;
                    Object relates2[] = new Object[n + 2];
                    System.arraycopy(relates, 0, relates2, 0, n);
                    relates2[n] = second;
                    relates2[n + 1] = width;
                    KernPairs.put(first, relates2);
                }
            }
            else if (ident.equals("EndKernPairs"))
            {
                isMetrics = false;
                break;
            }
        }
        if (isMetrics)
            throw new DocumentException("Missing EndKernPairs in " + fileName);
        rf.close();
    }
    

    private PdfStream getFontStream() throws DocumentException
    {
        if (builtinFont || !embedded)
            return null;
        RandomAccessFileOrArray rf = null;
        try {
            String filePfb = fileName.substring(0, fileName.length() - 3) + "pfb";
            if (pfb == null)
                rf = new RandomAccessFileOrArray(filePfb);
            else
                rf = new RandomAccessFileOrArray(pfb);
            int fileLength = rf.length();
            byte st[] = new byte[fileLength - 18];
            int lengths[] = new int[3];
            int bytePtr = 0;
            for (int k = 0; k < 3; ++k) {
                if (rf.read() != 0x80)
                    throw new DocumentException("Start marker missing in " + filePfb);
                if (rf.read() != PFB_TYPES[k])
                    throw new DocumentException("Incorrect segment type in " + filePfb);
                int size = rf.read();
                size += rf.read() << 8;
                size += rf.read() << 16;
                size += rf.read() << 24;
                lengths[k] = size;
                while (size != 0) {
                    int got = rf.read(st, bytePtr, size);
                    if (got < 0)
                        throw new DocumentException("Premature end in " + filePfb);
                    bytePtr += got;
                    size -= got;
                }
            }
            return new StreamFont(st, lengths);
        }
        catch (Exception e) {
            throw new DocumentException(e);
        }
        finally {
            if (rf != null) {
                try {
                    rf.close();
                }
                catch (Exception e) {
                    
                }
            }
        }
    }
    

    private PdfDictionary getFontDescriptor(PdfIndirectReference fontStream)
    {
        if (builtinFont)
            return null;
        PdfDictionary dic = new PdfDictionary(PdfName.FONTDESCRIPTOR);
        dic.put(PdfName.ASCENT, new PdfNumber(Ascender));
        dic.put(PdfName.CAPHEIGHT, new PdfNumber(CapHeight));
        dic.put(PdfName.DESCENT, new PdfNumber(Descender));
        dic.put(PdfName.FONTBBOX, new PdfRectangle(llx, lly, urx, ury));
        dic.put(PdfName.FONTNAME, new PdfName(FontName));
        dic.put(PdfName.ITALICANGLE, new PdfNumber(ItalicAngle));
        dic.put(PdfName.STEMV, new PdfNumber(StdVW));
        if (fontStream != null)
            dic.put(PdfName.FONTFILE, fontStream);
        int flags = 0;
        if (IsFixedPitch)
            flags |= 1;
        flags |= fontSpecific ? 4 : 32;
        if (ItalicAngle < 0)
            flags |= 64;
        if (FontName.indexOf("Caps") >= 0 || FontName.endsWith("SC"))
            flags |= 131072;
        if (Weight.equals("Bold"))
            flags |= 262144;
        dic.put(PdfName.FLAGS, new PdfNumber(flags));
        
        return dic;
    }
    
    
    private PdfDictionary getFontBaseType(PdfIndirectReference fontDescriptor, int firstChar, int lastChar, byte shortTag[])
    {
        PdfDictionary dic = new PdfDictionary(PdfName.FONT);
        dic.put(PdfName.SUBTYPE, PdfName.TYPE1);
        dic.put(PdfName.BASEFONT, new PdfName(FontName));
        boolean stdEncoding = encoding.equals("Cp1252") || encoding.equals("MacRoman");
        if (!fontSpecific || specialMap != null) {
            for (int k = firstChar; k <= lastChar; ++k) {
                if (!differences[k].equals(notdef)) {
                    firstChar = k;
                    break;
                }
            }
            if (stdEncoding)
                dic.put(PdfName.ENCODING, encoding.equals("Cp1252") ? PdfName.WIN_ANSI_ENCODING : PdfName.MAC_ROMAN_ENCODING);
            else {
                PdfDictionary enc = new PdfDictionary(PdfName.ENCODING);
                PdfArray dif = new PdfArray();
                boolean gap = true;                
                for (int k = firstChar; k <= lastChar; ++k) {
                    if (shortTag[k] != 0) {
                        if (gap) {
                            dif.add(new PdfNumber(k));
                            gap = false;
                        }
                        dif.add(new PdfName(differences[k]));
                    }
                    else
                        gap = true;
                }
                enc.put(PdfName.DIFFERENCES, dif);
                dic.put(PdfName.ENCODING, enc);
            }
        }
        if (specialMap != null || forceWidthsOutput || !(builtinFont && (fontSpecific || stdEncoding))) {
            dic.put(PdfName.FIRSTCHAR, new PdfNumber(firstChar));
            dic.put(PdfName.LASTCHAR, new PdfNumber(lastChar));
            PdfArray wd = new PdfArray();
            for (int k = firstChar; k <= lastChar; ++k) {
                if (shortTag[k] == 0)
                    wd.add(new PdfNumber(0));
                else
                    wd.add(new PdfNumber(widths[k]));
            }
            dic.put(PdfName.WIDTHS, wd);
        }
        if (!builtinFont && fontDescriptor != null)
            dic.put(PdfName.FONTDESCRIPTOR, fontDescriptor);
        return dic;
    }
    
    
    void writeFont(PdfWriter writer, PdfIndirectReference ref, Object params[]) throws DocumentException, IOException {
        int firstChar = ((Integer)params[0]).intValue();
        int lastChar = ((Integer)params[1]).intValue();
        byte shortTag[] = (byte[])params[2];
        boolean subsetp = ((Boolean)params[3]).booleanValue() && subset;
        if (!subsetp) {
            firstChar = 0;
            lastChar = shortTag.length - 1;
            for (int k = 0; k < shortTag.length; ++k)
                shortTag[k] = 1;
        }
        PdfIndirectReference ind_font = null;
        PdfObject pobj = null;
        PdfIndirectObject obj = null;
        pobj = getFontStream();
        if (pobj != null){
            obj = writer.addToBody(pobj);
            ind_font = obj.getIndirectReference();
        }
        pobj = getFontDescriptor(ind_font);
        if (pobj != null){
            obj = writer.addToBody(pobj);
            ind_font = obj.getIndirectReference();
        }
        pobj = getFontBaseType(ind_font, firstChar, lastChar, shortTag);
        writer.addToBody(pobj, ref);
    }
    
        
    public float getFontDescriptor(int key, float fontSize) {
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
            case UNDERLINE_POSITION:
                return UnderlinePosition * fontSize / 1000;
            case UNDERLINE_THICKNESS:
                return UnderlineThickness * fontSize / 1000;
        }
        return 0;
    }
    
    
    public String getPostscriptFontName() {
        return FontName;
    }
    
    
    public String[][] getFullFontName() {
        return new String[][]{{"", "", "", FullName}};
    }
    
    
    public String[][] getAllNameEntries() {
        return new String[][]{{"4", "", "", "", FullName}};
    }
    
    
    public String[][] getFamilyFontName() {
        return new String[][]{{"", "", "", FamilyName}};
    }
    
        
    public boolean hasKernPairs() {
        return !KernPairs.isEmpty();
    }
    
        
    public void setPostscriptFontName(String name) {
        FontName = name;
    }
    
    
    public boolean setKerning(int char1, int char2, int kern) {
        String first = GlyphList.unicodeToName(char1);
        if (first == null)
            return false;
        String second = GlyphList.unicodeToName(char2);
        if (second == null)
            return false;
        Object obj[] = KernPairs.get(first);
        if (obj == null) {
            obj = new Object[]{second, new Integer(kern)};
            KernPairs.put(first, obj);
            return true;
        }
        for (int k = 0; k < obj.length; k += 2) {
            if (second.equals(obj[k])) {
                obj[k + 1] = new Integer(kern);
                return true;
            }
        }
        int size = obj.length;
        Object obj2[] = new Object[size + 2];
        System.arraycopy(obj, 0, obj2, 0, size);
        obj2[size] = second;
        obj2[size + 1] = new Integer(kern);
        KernPairs.put(first, obj2);
        return true;
    }
    
    protected int[] getRawCharBBox(int c, String name) {
        Object metrics[];
        if (name == null) { 
            metrics = CharMetrics.get(new Integer(c));
        }
        else {
            if (name.equals(".notdef"))
                return null;
            metrics = CharMetrics.get(name);
        }
        if (metrics != null)
            return ((int[])(metrics[3]));
        return null;
    }
    
}
