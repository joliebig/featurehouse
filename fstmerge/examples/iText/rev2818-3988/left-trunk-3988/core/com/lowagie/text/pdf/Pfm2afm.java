



 







package com.lowagie.text.pdf;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;


public final class Pfm2afm {
    private RandomAccessFileOrArray in;
    private PrintWriter out;
    
    
    private Pfm2afm(RandomAccessFileOrArray in, OutputStream out) throws IOException {
        this.in = in;
        this.out = new PrintWriter(new OutputStreamWriter(out, "ISO-8859-1"));
    }
    
        
    public static void convert(RandomAccessFileOrArray in, OutputStream out) throws IOException {
        Pfm2afm p = new Pfm2afm(in, out);
        p.openpfm();
        p.putheader();
        p.putchartab();
        p.putkerntab();
        p.puttrailer();
        p.out.flush();
    }
    
    public static void main(String[] args) {
        try {
            RandomAccessFileOrArray in = new RandomAccessFileOrArray(args[0]);
            OutputStream out = new FileOutputStream(args[1]);
            convert(in, out);
            in.close();
            out.close();
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    private String readString(int n) throws IOException {
        byte b[] = new byte[n];
        in.readFully(b);
        int k;
        for (k = 0; k < b.length; ++k) {
            if (b[k] == 0)
                break;
        }
        return new String(b, 0, k, "ISO-8859-1");
    }
    
    private String readString() throws IOException {
        StringBuffer buf = new StringBuffer();
        while (true) {
            int c = in.read();
            if (c <= 0)
                break;
            buf.append((char)c);
        }
        return buf.toString();
    }
    
    private void outval(int n) {
        out.print(' ');
        out.print(n);
    }
    
    
    private void  outchar(int code, int width, String name) {
        out.print("C ");
        outval(code);
        out.print(" ; WX ");
        outval(width);
        if (name != null) {
            out.print(" ; N ");
            out.print(name);
        }
        out.print(" ;\n");
    }
    
    private void openpfm() throws IOException {
        in.seek(0);
        vers = in.readShortLE();
        h_len = in.readIntLE();
        copyright = readString(60);
        type = in.readShortLE();
        points = in.readShortLE();
        verres = in.readShortLE();
        horres = in.readShortLE();
        ascent = in.readShortLE();
        intleading = in.readShortLE();
        extleading = in.readShortLE();
        italic = (byte)in.read();
        uline = (byte)in.read();
        overs = (byte)in.read();
        weight = in.readShortLE();
        charset = (byte)in.read();
        pixwidth = in.readShortLE();
        pixheight = in.readShortLE();
        kind = (byte)in.read();
        avgwidth = in.readShortLE();
        maxwidth = in.readShortLE();
        firstchar = in.read();
        lastchar = in.read();
        defchar = (byte)in.read();
        brkchar = (byte)in.read();
        widthby = in.readShortLE();
        device = in.readIntLE();
        face = in.readIntLE();
        bits = in.readIntLE();
        bitoff = in.readIntLE();
        extlen = in.readShortLE();
        psext = in.readIntLE();
        chartab = in.readIntLE();
        res1 = in.readIntLE();
        kernpairs = in.readIntLE();
        res2 = in.readIntLE();
        fontname = in.readIntLE();
        if (h_len != in.length() || extlen != 30 || fontname < 75 || fontname > 512)
            throw new IOException("Not a valid PFM file.");
        in.seek(psext + 14);
        capheight = in.readShortLE();
        xheight = in.readShortLE();
        ascender = in.readShortLE();
        descender = in.readShortLE();
    }
    
    private void putheader() throws IOException {
        out.print("StartFontMetrics 2.0\n");
        if (copyright.length() > 0)
            out.print("Comment " + copyright + '\n');
        out.print("FontName ");
        in.seek(fontname);
        String fname = readString();
        out.print(fname);
        out.print("\nEncodingScheme ");
        if (charset != 0)
            out.print("FontSpecific\n");
        else
            out.print("AdobeStandardEncoding\n");
        
        out.print("FullName " + fname.replace('-', ' '));
        if (face != 0) {
            in.seek(face);
            out.print("\nFamilyName " + readString());
        }

        out.print("\nWeight ");
        if (weight > 475 || fname.toLowerCase().indexOf("bold") >= 0)
           out.print("Bold");
        else if ((weight < 325 && weight != 0) || fname.toLowerCase().indexOf("light") >= 0)
            out.print("Light");
        else if (fname.toLowerCase().indexOf("black") >= 0)
            out.print("Black");
        else 
            out.print("Medium");

        out.print("\nItalicAngle ");
        if (italic != 0 || fname.toLowerCase().indexOf("italic") >= 0)
            out.print("-12.00");
            
        else
            out.print("0");

        
        out.print("\nIsFixedPitch ");
        if ((kind & 1) == 0 ||                  
            avgwidth == maxwidth ) {  
            out.print("true");
            isMono = true;
        }
        else {
            out.print("false");
            isMono = false;
        }

        
        out.print("\nFontBBox");
        if (isMono)
            outval(-20);      
        else 
            outval(-100);
        outval(-(descender+5));  
        outval(maxwidth+10);
        outval(ascent+5);

        
        out.print("\nCapHeight");
        outval(capheight);
        out.print("\nXHeight");
        outval(xheight);
        out.print("\nDescender");
        outval(descender);
        out.print("\nAscender");
        outval(ascender);
        out.print('\n');
    }
    
    private void putchartab() throws IOException {
        int count = lastchar - firstchar + 1;
        int ctabs[] = new int[count];
        in.seek(chartab);
        for (int k = 0; k < count; ++k)
            ctabs[k] = in.readUnsignedShortLE();
        int back[] = new int[256];
        if (charset == 0) {
            for (int i = firstchar; i <= lastchar; ++i) {
                if (Win2PSStd[i] != 0)
                    back[Win2PSStd[i]] = i;
            }
        }
        
        out.print("StartCharMetrics");
        outval(count);
        out.print('\n');

        
        if (charset != 0) {
        
            for (int i = firstchar; i <= lastchar; i++) {
                if (ctabs[i - firstchar] != 0) {
                    outchar(i, ctabs[i - firstchar], null);
                }
            }
        }
        else {
            for (int i = 0; i < 256; i++) {
                int j = back[i];
                if (j != 0) {
                    outchar(i, ctabs[j - firstchar], WinChars[j]);
                    ctabs[j - firstchar] = 0;
                }
            }
            
            for (int i = firstchar; i <= lastchar; i++) {
                if (ctabs[i - firstchar] != 0) {
                    outchar(-1, ctabs[i - firstchar], WinChars[i]);
                }
            }
        }
        
        out.print("EndCharMetrics\n");
        
    }
    
    private void putkerntab() throws IOException {
        if (kernpairs == 0)
            return;
        in.seek(kernpairs);
        int count = in.readUnsignedShortLE();
        int nzero = 0;
        int kerns[] = new int[count * 3];
        for (int k = 0; k < kerns.length;) {
            kerns[k++] = in.read();
            kerns[k++] = in.read();
            if ((kerns[k++] = in.readShortLE()) != 0)
                ++nzero;
        }
        if (nzero == 0)
            return;
        out.print("StartKernData\nStartKernPairs");
        outval(nzero);
        out.print('\n');
        for (int k = 0; k < kerns.length; k += 3) {
            if (kerns[k + 2] != 0) {
                out.print("KPX ");
                out.print(WinChars[kerns[k]]);
                out.print(' ');
                out.print(WinChars[kerns[k + 1]]);
                outval(kerns[k + 2]);
                out.print('\n');
            }
        }
        
        out.print("EndKernPairs\nEndKernData\n");
    }
    

    private void  puttrailer() {
        out.print("EndFontMetrics\n");
    }

    private short  vers;
    private int   h_len;             
    private String   copyright;   
    private short  type;
    private short  points;
    private short  verres;
    private short  horres;
    private short  ascent;
    private short  intleading;
    private short  extleading;
    private byte   italic;
    private byte   uline;
    private byte   overs;
    private short  weight;
    private byte   charset;         
    private short  pixwidth;        
    private short  pixheight;
    private byte   kind;            
    private short  avgwidth;        
    private short  maxwidth;        
    private int   firstchar;       
    private int   lastchar;        
    private byte   defchar;
    private byte   brkchar;
    private short  widthby;
    private int   device;
    private int   face;            
    private int   bits;
    private int   bitoff;
    private short  extlen;
    private int   psext;           
    private int   chartab;         
    private int   res1;
    private int   kernpairs;       
    private int   res2;
    private int   fontname;        


    private short  capheight;       
    private short  xheight;         
    private short  ascender;        
    private short  descender;       

    
    private boolean isMono;

    private int Win2PSStd[] = {
        0,   0,   0,   0, 197, 198, 199,   0, 202,   0,   205, 206, 207, 0,   0,   0,   
        0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   
        32,  33,  34,  35,  36,  37,  38, 169,  40,  41,  42,  43,  44,  45,  46,  47,  
        48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,  
        64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,  
        80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,  
        193, 97,  98,  99,  100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 
        112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 
        0,   0,   184, 166, 185, 188, 178, 179, 195, 189, 0,   172, 234, 0,   0,   0,   
        0,   96,  0,   170, 186, 183, 177, 208, 196, 0,   0,   173, 250, 0,   0,   0,   
        0,   161, 162, 163, 168, 165, 0,   167, 200, 0,   227, 171, 0,   0,   0,   197, 
        0,   0,   0,   0,   194, 0,   182, 180, 203, 0,   235, 187, 0,   0,   0,   191, 
        0,   0,   0,   0,   0,   0,   225, 0,   0,   0,   0,   0,   0,   0,   0,   0,   
        0,   0,   0,   0,   0,   0,   0,   0,   233, 0,   0,   0,   0,   0,   0,   251, 
        0,   0,   0,   0,   0,   0,   241, 0,   0,   0,   0,   0,   0,   0,   0,   0,   
        0,   0,   0,   0,   0,   0,   0,   0,   249, 0,   0,   0,   0,   0,   0,   0    
    };
    

    private int WinClass[] = {
        0, 0, 0, 0, 2, 2, 2, 0, 2, 0, 2, 2, 2, 0, 0, 0,   
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2,   
        0, 0, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0,   
        0, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 2,   
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1   
    };
    

    private String WinChars[] = {
        "W00",              
        "W01",              
        "W02",              
        "W03",              
        "macron",           
        "breve",            
        "dotaccent",        
        "W07",              
        "ring",             
        "W09",              
        "W0a",              
        "W0b",              
        "W0c",              
        "W0d",              
        "W0e",              
        "W0f",              
        "hungarumlaut",     
        "ogonek",           
        "caron",            
        "W13",              
        "W14",              
        "W15",              
        "W16",              
        "W17",              
        "W18",              
        "W19",              
        "W1a",              
        "W1b",              
        "W1c",              
        "W1d",              
        "W1e",              
        "W1f",              
        "space",            
        "exclam",           
        "quotedbl",         
        "numbersign",       
        "dollar",           
        "percent",          
        "ampersand",        
        "quotesingle",      
        "parenleft",        
        "parenright",       
        "asterisk",         
        "plus",             
        "comma",            
        "hyphen",           
        "period",           
        "slash",            
        "zero",             
        "one",              
        "two",              
        "three",            
        "four",             
        "five",             
        "six",              
        "seven",            
        "eight",            
        "nine",             
        "colon",            
        "semicolon",        
        "less",             
        "equal",            
        "greater",          
        "question",         
        "at",               
        "A",                
        "B",                
        "C",                
        "D",                
        "E",                
        "F",                
        "G",                
        "H",                
        "I",                
        "J",                
        "K",                
        "L",                
        "M",                
        "N",                
        "O",                
        "P",                
        "Q",                
        "R",                
        "S",                
        "T",                
        "U",                
        "V",                
        "W",                
        "X",                
        "Y",                
        "Z",                
        "bracketleft",      
        "backslash",        
        "bracketright",     
        "asciicircum",      
        "underscore",       
        "grave",            
        "a",                
        "b",                
        "c",                
        "d",                
        "e",                
        "f",                
        "g",                
        "h",                
        "i",                
        "j",                
        "k",                
        "l",                
        "m",                
        "n",                
        "o",                
        "p",                
        "q",                
        "r",                
        "s",                
        "t",                
        "u",                
        "v",                
        "w",                
        "x",                
        "y",                
        "z",                
        "braceleft",        
        "bar",              
        "braceright",       
        "asciitilde",       
        "W7f",              
        "euro",             
        "W81",              
        "quotesinglbase",   
        "florin",           
        "quotedblbase",     
        "ellipsis",         
        "dagger",           
        "daggerdbl",        
        "circumflex",       
        "perthousand",      
        "Scaron",           
        "guilsinglleft",    
        "OE",               
        "W8d",              
        "Zcaron",           
        "W8f",              
        "W90",              
        "quoteleft",        
        "quoteright",       
        "quotedblleft",     
        "quotedblright",    
        "bullet",           
        "endash",           
        "emdash",           
        "tilde",            
        "trademark",        
        "scaron",           
        "guilsinglright",   
        "oe",               
        "W9d",              
        "zcaron",           
        "Ydieresis",        
        "reqspace",         
        "exclamdown",       
        "cent",             
        "sterling",         
        "currency",         
        "yen",              
        "brokenbar",        
        "section",          
        "dieresis",         
        "copyright",        
        "ordfeminine",      
        "guillemotleft",    
        "logicalnot",       
        "syllable",         
        "registered",       
        "macron",           
        "degree",           
        "plusminus",        
        "twosuperior",      
        "threesuperior",    
        "acute",            
        "mu",               
        "paragraph",        
        "periodcentered",   
        "cedilla",          
        "onesuperior",      
        "ordmasculine",     
        "guillemotright",   
        "onequarter",       
        "onehalf",          
        "threequarters",    
        "questiondown",     
        "Agrave",           
        "Aacute",           
        "Acircumflex",      
        "Atilde",           
        "Adieresis",        
        "Aring",            
        "AE",               
        "Ccedilla",         
        "Egrave",           
        "Eacute",           
        "Ecircumflex",      
        "Edieresis",        
        "Igrave",           
        "Iacute",           
        "Icircumflex",      
        "Idieresis",        
        "Eth",              
        "Ntilde",           
        "Ograve",           
        "Oacute",           
        "Ocircumflex",      
        "Otilde",           
        "Odieresis",        
        "multiply",         
        "Oslash",           
        "Ugrave",           
        "Uacute",           
        "Ucircumflex",      
        "Udieresis",        
        "Yacute",           
        "Thorn",            
        "germandbls",       
        "agrave",           
        "aacute",           
        "acircumflex",      
        "atilde",           
        "adieresis",        
        "aring",            
        "ae",               
        "ccedilla",         
        "egrave",           
        "eacute",           
        "ecircumflex",      
        "edieresis",        
        "igrave",           
        "iacute",           
        "icircumflex",      
        "idieresis",        
        "eth",              
        "ntilde",           
        "ograve",           
        "oacute",           
        "ocircumflex",      
        "otilde",           
        "odieresis",        
        "divide",           
        "oslash",           
        "ugrave",           
        "uacute",           
        "ucircumflex",      
        "udieresis",        
        "yacute",           
        "thorn",            
        "ydieresis"         
    };
}