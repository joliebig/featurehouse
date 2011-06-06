



package com.lowagie.text.pdf;

import java.util.LinkedList;

import com.lowagie.text.ExceptionConverter;

public class CFFFont {
    
    static final String operatorNames[] = {
        "version", "Notice", "FullName", "FamilyName",
        "Weight", "FontBBox", "BlueValues", "OtherBlues",
        "FamilyBlues", "FamilyOtherBlues", "StdHW", "StdVW",
        "UNKNOWN_12", "UniqueID", "XUID", "charset",
        "Encoding", "CharStrings", "Private", "Subrs",
        "defaultWidthX", "nominalWidthX", "UNKNOWN_22", "UNKNOWN_23",
        "UNKNOWN_24", "UNKNOWN_25", "UNKNOWN_26", "UNKNOWN_27",
        "UNKNOWN_28", "UNKNOWN_29", "UNKNOWN_30", "UNKNOWN_31",
        "Copyright", "isFixedPitch", "ItalicAngle", "UnderlinePosition",
        "UnderlineThickness", "PaintType", "CharstringType", "FontMatrix",
        "StrokeWidth", "BlueScale", "BlueShift", "BlueFuzz",
        "StemSnapH", "StemSnapV", "ForceBold", "UNKNOWN_12_15",
        "UNKNOWN_12_16", "LanguageGroup", "ExpansionFactor", "initialRandomSeed",
        "SyntheticBase", "PostScript", "BaseFontName", "BaseFontBlend",
        "UNKNOWN_12_24", "UNKNOWN_12_25", "UNKNOWN_12_26", "UNKNOWN_12_27",
        "UNKNOWN_12_28", "UNKNOWN_12_29", "ROS", "CIDFontVersion",
        "CIDFontRevision", "CIDFontType", "CIDCount", "UIDBase",
        "FDArray", "FDSelect", "FontName"
    };
    
    static final String standardStrings[] = {
        
        
        ".notdef", "space", "exclam", "quotedbl", "numbersign", "dollar",
        "percent", "ampersand", "quoteright", "parenleft", "parenright",
        "asterisk", "plus", "comma", "hyphen", "period", "slash", "zero", "one",
        "two", "three", "four", "five", "six", "seven", "eight", "nine", "colon",
        "semicolon", "less", "equal", "greater", "question", "at", "A", "B", "C",
        "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R",
        "S", "T", "U", "V", "W", "X", "Y", "Z", "bracketleft", "backslash",
        "bracketright", "asciicircum", "underscore", "quoteleft", "a", "b", "c",
        "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r",
        "s", "t", "u", "v", "w", "x", "y", "z", "braceleft", "bar", "braceright",
        "asciitilde", "exclamdown", "cent", "sterling", "fraction", "yen",
        "florin", "section", "currency", "quotesingle", "quotedblleft",
        "guillemotleft", "guilsinglleft", "guilsinglright", "fi", "fl", "endash",
        "dagger", "daggerdbl", "periodcentered", "paragraph", "bullet",
        "quotesinglbase", "quotedblbase", "quotedblright", "guillemotright",
        "ellipsis", "perthousand", "questiondown", "grave", "acute", "circumflex",
        "tilde", "macron", "breve", "dotaccent", "dieresis", "ring", "cedilla",
        "hungarumlaut", "ogonek", "caron", "emdash", "AE", "ordfeminine", "Lslash",
        "Oslash", "OE", "ordmasculine", "ae", "dotlessi", "lslash", "oslash", "oe",
        "germandbls", "onesuperior", "logicalnot", "mu", "trademark", "Eth",
        "onehalf", "plusminus", "Thorn", "onequarter", "divide", "brokenbar",
        "degree", "thorn", "threequarters", "twosuperior", "registered", "minus",
        "eth", "multiply", "threesuperior", "copyright", "Aacute", "Acircumflex",
        "Adieresis", "Agrave", "Aring", "Atilde", "Ccedilla", "Eacute",
        "Ecircumflex", "Edieresis", "Egrave", "Iacute", "Icircumflex", "Idieresis",
        "Igrave", "Ntilde", "Oacute", "Ocircumflex", "Odieresis", "Ograve",
        "Otilde", "Scaron", "Uacute", "Ucircumflex", "Udieresis", "Ugrave",
        "Yacute", "Ydieresis", "Zcaron", "aacute", "acircumflex", "adieresis",
        "agrave", "aring", "atilde", "ccedilla", "eacute", "ecircumflex",
        "edieresis", "egrave", "iacute", "icircumflex", "idieresis", "igrave",
        "ntilde", "oacute", "ocircumflex", "odieresis", "ograve", "otilde",
        "scaron", "uacute", "ucircumflex", "udieresis", "ugrave", "yacute",
        "ydieresis", "zcaron", "exclamsmall", "Hungarumlautsmall",
        "dollaroldstyle", "dollarsuperior", "ampersandsmall", "Acutesmall",
        "parenleftsuperior", "parenrightsuperior", "twodotenleader",
        "onedotenleader", "zerooldstyle", "oneoldstyle", "twooldstyle",
        "threeoldstyle", "fouroldstyle", "fiveoldstyle", "sixoldstyle",
        "sevenoldstyle", "eightoldstyle", "nineoldstyle", "commasuperior",
        "threequartersemdash", "periodsuperior", "questionsmall", "asuperior",
        "bsuperior", "centsuperior", "dsuperior", "esuperior", "isuperior",
        "lsuperior", "msuperior", "nsuperior", "osuperior", "rsuperior",
        "ssuperior", "tsuperior", "ff", "ffi", "ffl", "parenleftinferior",
        "parenrightinferior", "Circumflexsmall", "hyphensuperior", "Gravesmall",
        "Asmall", "Bsmall", "Csmall", "Dsmall", "Esmall", "Fsmall", "Gsmall",
        "Hsmall", "Ismall", "Jsmall", "Ksmall", "Lsmall", "Msmall", "Nsmall",
        "Osmall", "Psmall", "Qsmall", "Rsmall", "Ssmall", "Tsmall", "Usmall",
        "Vsmall", "Wsmall", "Xsmall", "Ysmall", "Zsmall", "colonmonetary",
        "onefitted", "rupiah", "Tildesmall", "exclamdownsmall", "centoldstyle",
        "Lslashsmall", "Scaronsmall", "Zcaronsmall", "Dieresissmall", "Brevesmall",
        "Caronsmall", "Dotaccentsmall", "Macronsmall", "figuredash",
        "hypheninferior", "Ogoneksmall", "Ringsmall", "Cedillasmall",
        "questiondownsmall", "oneeighth", "threeeighths", "fiveeighths",
        "seveneighths", "onethird", "twothirds", "zerosuperior", "foursuperior",
        "fivesuperior", "sixsuperior", "sevensuperior", "eightsuperior",
        "ninesuperior", "zeroinferior", "oneinferior", "twoinferior",
        "threeinferior", "fourinferior", "fiveinferior", "sixinferior",
        "seveninferior", "eightinferior", "nineinferior", "centinferior",
        "dollarinferior", "periodinferior", "commainferior", "Agravesmall",
        "Aacutesmall", "Acircumflexsmall", "Atildesmall", "Adieresissmall",
        "Aringsmall", "AEsmall", "Ccedillasmall", "Egravesmall", "Eacutesmall",
        "Ecircumflexsmall", "Edieresissmall", "Igravesmall", "Iacutesmall",
        "Icircumflexsmall", "Idieresissmall", "Ethsmall", "Ntildesmall",
        "Ogravesmall", "Oacutesmall", "Ocircumflexsmall", "Otildesmall",
        "Odieresissmall", "OEsmall", "Oslashsmall", "Ugravesmall", "Uacutesmall",
        "Ucircumflexsmall", "Udieresissmall", "Yacutesmall", "Thornsmall",
        "Ydieresissmall", "001.000", "001.001", "001.002", "001.003", "Black",
        "Bold", "Book", "Light", "Medium", "Regular", "Roman", "Semibold"
    };
    
    
    public String getString(char sid) {
        if (sid < standardStrings.length) return standardStrings[sid];
        if (sid >= standardStrings.length+(stringOffsets.length-1)) return null;
        int j = sid - standardStrings.length;
        
        int p = getPosition();
        seek(stringOffsets[j]);
        StringBuffer s = new StringBuffer();
        for (int k=stringOffsets[j]; k<stringOffsets[j+1]; k++) {
            s.append(getCard8());
        }
        seek(p);
        return s.toString();
    }
    
    char getCard8() {
        try {
            byte i = buf.readByte();
            return (char)(i & 0xff);
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
    char getCard16() {
        try {
            return buf.readChar();
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
    int getOffset(int offSize) {
        int offset = 0;
        for (int i=0; i<offSize; i++) {
            offset *= 256;
            offset += getCard8();
        }
        return offset;
    }
    
    void seek(int offset) {
        try {
            buf.seek(offset);
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
    short getShort() {
        try {
            return buf.readShort();
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
    int getInt() {
        try {
            return buf.readInt();
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
    int getPosition() {
        try {
            return buf.getFilePointer();
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    int nextIndexOffset;
    
    
    
    
    int[] getIndex(int nextIndexOffset) {
        int count, indexOffSize;
        
        seek(nextIndexOffset);
        count = getCard16();
        int[] offsets = new int[count+1];
        
        if (count==0) {
            offsets[0] = -1;
            nextIndexOffset += 2;
            return offsets;
        }
        
        indexOffSize = getCard8();
        
        for (int j=0; j<=count; j++) {
            
            offsets[j] = nextIndexOffset
            
            + 2+1
            
            + (count+1)*indexOffSize
            
            - 1
            
            + getOffset(indexOffSize);
        }
        
        return offsets;
    }
    
    protected String   key;
    protected Object[] args      = new Object[48];
    protected int      arg_count = 0;
    
    protected void getDictItem() {
        for (int i=0; i<arg_count; i++) args[i]=null;
        arg_count = 0;
        key = null;
        boolean gotKey = false;
        
        while (!gotKey) {
            char b0 = getCard8();
            if (b0 == 29) {
                int item = getInt();
                args[arg_count] = new Integer(item);
                arg_count++;
                
                continue;
            }
            if (b0 == 28) {
                short item = getShort();
                args[arg_count] = new Integer(item);
                arg_count++;
                
                continue;
            }
            if (b0 >= 32 && b0 <= 246) {
                byte item = (byte) (b0-139);
                args[arg_count] = new Integer(item);
                arg_count++;
                
                continue;
            }
            if (b0 >= 247 && b0 <= 250) {
                char b1 = getCard8();
                short item = (short) ((b0-247)*256+b1+108);
                args[arg_count] = new Integer(item);
                arg_count++;
                
                continue;
            }
            if (b0 >= 251 && b0 <= 254) {
                char b1 = getCard8();
                short item = (short) (-(b0-251)*256-b1-108);
                args[arg_count] = new Integer(item);
                arg_count++;
                
                continue;
            }
            if (b0 == 30) {
                String item = "";
                boolean done = false;
                char buffer = 0;
                byte avail = 0;
                int  nibble = 0;
                while (!done) {
                    
                    if (avail==0) { buffer = getCard8(); avail=2; }
                    if (avail==1) { nibble = (buffer / 16); avail--; }
                    if (avail==2) { nibble = (buffer % 16); avail--; }
                    switch (nibble) {
                        case 0xa: item += "." ; break;
                        case 0xb: item += "E" ; break;
                        case 0xc: item += "E-"; break;
                        case 0xe: item += "-" ; break;
                        case 0xf: done=true   ; break;
                        default:
                            if (nibble >= 0 && nibble <= 9)
                                item += String.valueOf(nibble);
                            else {
                                item += "<NIBBLE ERROR: " + nibble + '>';
                                done = true;
                            }
                            break;
                    }
                }
                args[arg_count] = item;
                arg_count++;
                
                continue;
            }
            if (b0 <= 21) {
                gotKey=true;
                if (b0 != 12) key = operatorNames[b0];
                else key = operatorNames[32 + getCard8()];
                
                
                
                continue;
            }
        }
    }
    
    
    
    protected static abstract class Item {
        protected int myOffset = -1;
        
        public void increment(int[] currentOffset) {
            myOffset = currentOffset[0];
        }
        
        public void emit(byte[] buffer) {}
        
        public void xref() {}
    }
    
    protected static abstract class OffsetItem extends Item {
        public int value;
        
        public void set(int offset) { this.value = offset; }
    }
    
    
    
    
    protected static final class RangeItem extends Item {
        public int offset, length;
        private RandomAccessFileOrArray buf;
        public RangeItem(RandomAccessFileOrArray buf, int offset, int length) {
            this.offset = offset;
            this.length = length;
            this.buf = buf;
        }
        public void increment(int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += length;
        }
        public void emit(byte[] buffer) {
            
            try {
                buf.seek(offset);
                for (int i=myOffset; i<myOffset+length; i++)
                    buffer[i] = buf.readByte();
            }
            catch (Exception e) {
                throw new ExceptionConverter(e);
            }
            
        }
    }
    
    
    static protected final class IndexOffsetItem extends OffsetItem {
        public final int size;
        public IndexOffsetItem(int size, int value) {this.size=size; this.value=value;}
        public IndexOffsetItem(int size) {this.size=size; }
        
        public void increment(int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += size;
        }
        public void emit(byte[] buffer) {
            int i=0;
            switch (size) {
                case 4:
                    buffer[myOffset+i] = (byte) ((value >>> 24) & 0xff);
                    i++;
                case 3:
                    buffer[myOffset+i] = (byte) ((value >>> 16) & 0xff);
                    i++;
                case 2:
                    buffer[myOffset+i] = (byte) ((value >>>  8) & 0xff);
                    i++;
                case 1:
                    buffer[myOffset+i] = (byte) ((value >>>  0) & 0xff);
                    i++;
            }
            
        }
    }
    
    static protected final class IndexBaseItem extends Item {
        public IndexBaseItem() {}
    }
    
    static protected final class IndexMarkerItem extends Item {
        private OffsetItem offItem;
        private IndexBaseItem indexBase;
        public IndexMarkerItem(OffsetItem offItem, IndexBaseItem indexBase) {
            this.offItem   = offItem;
            this.indexBase = indexBase;
        }
        public void xref() {
            
            offItem.set(this.myOffset-indexBase.myOffset+1);
        }
    }
    
    static protected final class SubrMarkerItem extends Item {
        private OffsetItem offItem;
        private IndexBaseItem indexBase;
        public SubrMarkerItem(OffsetItem offItem, IndexBaseItem indexBase) {
            this.offItem   = offItem;
            this.indexBase = indexBase;
        }
        public void xref() {
            
            offItem.set(this.myOffset-indexBase.myOffset);
        }
    }
    
    
    
    static protected final class DictOffsetItem extends OffsetItem {
        public final int size;
        public DictOffsetItem() {this.size=5; }
        
        public void increment(int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += size;
        }
        
        public void emit(byte[] buffer) {
            if (size==5) {
                buffer[myOffset]   = 29;
                buffer[myOffset+1] = (byte) ((value >>> 24) & 0xff);
                buffer[myOffset+2] = (byte) ((value >>> 16) & 0xff);
                buffer[myOffset+3] = (byte) ((value >>>  8) & 0xff);
                buffer[myOffset+4] = (byte) ((value >>>  0) & 0xff);
            }
        }
    }
    
    
    
    static protected final class UInt24Item extends Item {
        public int value;
        public UInt24Item(int value) {this.value=value;}
        
        public void increment(int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += 3;
        }
        
        public void emit(byte[] buffer) {
            buffer[myOffset+0] = (byte) ((value >>> 16) & 0xff);
            buffer[myOffset+1] = (byte) ((value >>> 8) & 0xff);
            buffer[myOffset+2] = (byte) ((value >>> 0) & 0xff);
        }
    }
    
    
    
    static protected final class UInt32Item extends Item {
        public int value;
        public UInt32Item(int value) {this.value=value;}
        
        public void increment(int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += 4;
        }
        
        public void emit(byte[] buffer) {
            buffer[myOffset+0] = (byte) ((value >>> 24) & 0xff);
            buffer[myOffset+1] = (byte) ((value >>> 16) & 0xff);
            buffer[myOffset+2] = (byte) ((value >>> 8) & 0xff);
            buffer[myOffset+3] = (byte) ((value >>> 0) & 0xff);
        }
    }

    
    
    static protected final class UInt16Item extends Item {
        public char value;
        public UInt16Item(char value) {this.value=value;}
        
        public void increment(int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += 2;
        }
        
        public void emit(byte[] buffer) {
            buffer[myOffset+0] = (byte) ((value >>> 8) & 0xff);
            buffer[myOffset+1] = (byte) ((value >>> 0) & 0xff);
        }
    }
    
    
    
    static protected final class UInt8Item extends Item {
        public char value;
        public UInt8Item(char value) {this.value=value;}
        
        public void increment(int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += 1;
        }
        
        public void emit(byte[] buffer) {
            buffer[myOffset+0] = (byte) ((value >>> 0) & 0xff);
        }
    }
    
    static protected final class StringItem extends Item {
        public String s;
        public StringItem(String s) {this.s=s;}
        
        public void increment(int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += s.length();
        }
        public void emit(byte[] buffer) {
            for (int i=0; i<s.length(); i++)
                buffer[myOffset+i] = (byte) (s.charAt(i) & 0xff);
        }
    }
    
    
    
    
    static protected final class DictNumberItem extends Item {
        public final int value;
        public int size = 5;
        public DictNumberItem(int value) {this.value=value;}
        public void increment(int[] currentOffset) {
            super.increment(currentOffset);
            currentOffset[0] += size;
        }
        
        public void emit(byte[] buffer) {
            if (size==5) {
                buffer[myOffset]   = 29;
                buffer[myOffset+1] = (byte) ((value >>> 24) & 0xff);
                buffer[myOffset+2] = (byte) ((value >>> 16) & 0xff);
                buffer[myOffset+3] = (byte) ((value >>>  8) & 0xff);
                buffer[myOffset+4] = (byte) ((value >>>  0) & 0xff);
            }
        }
    }
    
    
    
    static protected final class MarkerItem extends Item {
        OffsetItem p;
        public MarkerItem(OffsetItem pointerToMarker) {p=pointerToMarker;}
        public void xref() {
            p.set(this.myOffset);
        }
    }
    
    
    
    protected RangeItem getEntireIndexRange(int indexOffset) {
        seek(indexOffset);
        int count = getCard16();
        if (count==0) {
            return new RangeItem(buf,indexOffset,2);
        } else {
            int indexOffSize = getCard8();
            seek(indexOffset+2+1+count*indexOffSize);
            int size = getOffset(indexOffSize)-1;
            return new RangeItem(buf,indexOffset,
            2+1+(count+1)*indexOffSize+size);
        }
    }
    
    
    
    
    
    public byte[] getCID(String fontName)
    
    {
        int j;
        for (j=0; j<fonts.length; j++)
            if (fontName.equals(fonts[j].name)) break;
        if (j==fonts.length) return null;
        
        LinkedList<Item> l = new LinkedList<Item>();
        
        
        
        seek(0);
        
        int major = getCard8();
        int minor = getCard8();
        int hdrSize = getCard8();
        int offSize = getCard8();
        nextIndexOffset = hdrSize;
        
        l.addLast(new RangeItem(buf,0,hdrSize));
        
        int nglyphs=-1, nstrings=-1;
        if ( ! fonts[j].isCID ) {
            
            seek(fonts[j].charstringsOffset);
            nglyphs = getCard16();
            seek(stringIndexOffset);
            nstrings = getCard16()+standardStrings.length;
            
        }
        
        
        
        l.addLast(new UInt16Item((char)1)); 
        l.addLast(new UInt8Item((char)1)); 
        l.addLast(new UInt8Item((char)1)); 
        l.addLast(new UInt8Item((char)( 1+fonts[j].name.length() )));
        l.addLast(new StringItem(fonts[j].name));
        
        
        
        
        l.addLast(new UInt16Item((char)1)); 
        l.addLast(new UInt8Item((char)2)); 
        l.addLast(new UInt16Item((char)1)); 
        OffsetItem topdictIndex1Ref = new IndexOffsetItem(2);
        l.addLast(topdictIndex1Ref);
        IndexBaseItem topdictBase = new IndexBaseItem();
        l.addLast(topdictBase);
        
        
        
        
        
        
        OffsetItem charsetRef     = new DictOffsetItem();
        OffsetItem charstringsRef = new DictOffsetItem();
        OffsetItem fdarrayRef     = new DictOffsetItem();
        OffsetItem fdselectRef    = new DictOffsetItem();
        
        if ( !fonts[j].isCID ) {
            
            l.addLast(new DictNumberItem(nstrings));
            l.addLast(new DictNumberItem(nstrings+1));
            l.addLast(new DictNumberItem(0));
            l.addLast(new UInt8Item((char)12));
            l.addLast(new UInt8Item((char)30));
            
            l.addLast(new DictNumberItem(nglyphs));
            l.addLast(new UInt8Item((char)12));
            l.addLast(new UInt8Item((char)34));
            
            
        }
        
        
        l.addLast(fdarrayRef);
        l.addLast(new UInt8Item((char)12));
        l.addLast(new UInt8Item((char)36));
        
        l.addLast(fdselectRef);
        l.addLast(new UInt8Item((char)12));
        l.addLast(new UInt8Item((char)37));
        
        l.addLast(charsetRef);
        l.addLast(new UInt8Item((char)15));
        
        l.addLast(charstringsRef);
        l.addLast(new UInt8Item((char)17));
        
        seek(topdictOffsets[j]);
        while (getPosition() < topdictOffsets[j+1]) {
            int p1 = getPosition();
            getDictItem();
            int p2 = getPosition();
            if (key=="Encoding"
            || key=="Private"
            || key=="FDSelect"
            || key=="FDArray"
            || key=="charset"
            || key=="CharStrings"
            ) {
                
            } else {
                l.add(new RangeItem(buf,p1,p2-p1));
            }
        }
        
        l.addLast(new IndexMarkerItem(topdictIndex1Ref,topdictBase));
        
        
        
        
        
        if (fonts[j].isCID) {
            l.addLast(getEntireIndexRange(stringIndexOffset));
        } else {
            String fdFontName = fonts[j].name+"-OneRange";
            if (fdFontName.length() > 127)
                fdFontName = fdFontName.substring(0,127);
            String extraStrings = "Adobe"+"Identity"+fdFontName;
            
            int origStringsLen = stringOffsets[stringOffsets.length-1]
            - stringOffsets[0];
            int stringsBaseOffset = stringOffsets[0]-1;
            
            byte stringsIndexOffSize;
            if (origStringsLen+extraStrings.length() <= 0xff) stringsIndexOffSize = 1;
            else if (origStringsLen+extraStrings.length() <= 0xffff) stringsIndexOffSize = 2;
            else if (origStringsLen+extraStrings.length() <= 0xffffff) stringsIndexOffSize = 3;
            else stringsIndexOffSize = 4;
            
            l.addLast(new UInt16Item((char)((stringOffsets.length-1)+3))); 
            l.addLast(new UInt8Item((char)stringsIndexOffSize)); 
            for (int i=0; i<stringOffsets.length; i++)
                l.addLast(new IndexOffsetItem(stringsIndexOffSize,
                stringOffsets[i]-stringsBaseOffset));
            int currentStringsOffset = stringOffsets[stringOffsets.length-1]
            - stringsBaseOffset;
            
            currentStringsOffset += "Adobe".length();
            l.addLast(new IndexOffsetItem(stringsIndexOffSize,currentStringsOffset));
            currentStringsOffset += "Identity".length();
            l.addLast(new IndexOffsetItem(stringsIndexOffSize,currentStringsOffset));
            currentStringsOffset += fdFontName.length();
            l.addLast(new IndexOffsetItem(stringsIndexOffSize,currentStringsOffset));
            
            l.addLast(new RangeItem(buf,stringOffsets[0],origStringsLen));
            l.addLast(new StringItem(extraStrings));
        }
        
        
        
        l.addLast(getEntireIndexRange(gsubrIndexOffset));
        
        
        
        if (fonts[j].isCID) {
            
        } else {
            
            l.addLast(new MarkerItem(fdselectRef));
            l.addLast(new UInt8Item((char)3)); 
            l.addLast(new UInt16Item((char)1)); 
            
            l.addLast(new UInt16Item((char)0)); 
            l.addLast(new UInt8Item((char)0)); 
            
            l.addLast(new UInt16Item((char)nglyphs)); 
            
            
            
            
            l.addLast(new MarkerItem(charsetRef));
            l.addLast(new UInt8Item((char)2)); 
            
            l.addLast(new UInt16Item((char)1)); 
            l.addLast(new UInt16Item((char)(nglyphs-1))); 
            
            
            
            
            l.addLast(new MarkerItem(fdarrayRef));
            l.addLast(new UInt16Item((char)1));
            l.addLast(new UInt8Item((char)1)); 
            l.addLast(new UInt8Item((char)1)); 
            
            OffsetItem privateIndex1Ref = new IndexOffsetItem(1);
            l.addLast(privateIndex1Ref);
            IndexBaseItem privateBase = new IndexBaseItem();
            l.addLast(privateBase);
            
            
            
            
            
            
            
            
            l.addLast(new DictNumberItem(fonts[j].privateLength));
            OffsetItem privateRef = new DictOffsetItem();
            l.addLast(privateRef);
            l.addLast(new UInt8Item((char)18)); 
            
            l.addLast(new IndexMarkerItem(privateIndex1Ref,privateBase));
            
            
            
            l.addLast(new MarkerItem(privateRef));
            
            
            
            l.addLast(new RangeItem(buf,fonts[j].privateOffset,fonts[j].privateLength));
            if (fonts[j].privateSubrs >= 0) {
                
                l.addLast(getEntireIndexRange(fonts[j].privateSubrs));
            }
        }
        
        
        
        l.addLast(new MarkerItem(charstringsRef));
        l.addLast(getEntireIndexRange(fonts[j].charstringsOffset));
        
        
        
        int[] currentOffset = new int[1];
        currentOffset[0] = 0;
        
        for (Item item: l) {
            item.increment(currentOffset);
        }
        
        for (Item item: l) {
            item.xref();
        }
        
        int size = currentOffset[0];
        byte[] b = new byte[size];
        
        for (Item item: l) {
            item.emit(b);
        }
        
        return b;
    }
    
    
    public boolean isCID(String fontName) {
        int j;
        for (j=0; j<fonts.length; j++)
            if (fontName.equals(fonts[j].name)) return fonts[j].isCID;
        return false;
    }
    
    public boolean exists(String fontName) {
        int j;
        for (j=0; j<fonts.length; j++)
            if (fontName.equals(fonts[j].name)) return true;
        return false;
    }
    
    
    public String[] getNames() {
        String[] names = new String[ fonts.length ];
        for (int i=0; i<fonts.length; i++)
            names[i] = fonts[i].name;
        return names;
    }
    
    protected RandomAccessFileOrArray buf;
    private int offSize;
    
    protected int nameIndexOffset;
    protected int topdictIndexOffset;
    protected int stringIndexOffset;
    protected int gsubrIndexOffset;
    protected int[] nameOffsets;
    protected int[] topdictOffsets;
    protected int[] stringOffsets;
    protected int[] gsubrOffsets;
    
    
    protected final class Font {
        public String    name;
        public String    fullName;
        public boolean   isCID = false;
        public int       privateOffset     = -1; 
        public int       privateLength     = -1; 
        public int       privateSubrs      = -1;
        public int       charstringsOffset = -1;
        public int       encodingOffset    = -1;
        public int       charsetOffset     = -1;
        public int       fdarrayOffset     = -1; 
        public int       fdselectOffset    = -1; 
        public int[]     fdprivateOffsets;
        public int[]     fdprivateLengths;
        public int[]     fdprivateSubrs;
        
        
        public int nglyphs;
        public int nstrings;
        public int CharsetLength;
        public int[]    charstringsOffsets;
        public int[]    charset;
        public int[]     FDSelect;
        public int FDSelectLength;
        public int FDSelectFormat;
        public int         CharstringType = 2;
        public int FDArrayCount;
        public int FDArrayOffsize;
        public int[] FDArrayOffsets;
        public int[] PrivateSubrsOffset;
        public int[][] PrivateSubrsOffsetsArray;
        public int[]       SubrsOffsets;
    }
    
    protected Font[] fonts;
    
    public CFFFont(RandomAccessFileOrArray inputbuffer) {
        
        
        buf = inputbuffer;
        seek(0);
        
        int major, minor;
        major = getCard8();
        minor = getCard8();
        
        
        
        int hdrSize = getCard8();
        
        offSize = getCard8();
        
        
        
        
        
        nameIndexOffset    = hdrSize;
        nameOffsets        = getIndex(nameIndexOffset);
        topdictIndexOffset = nameOffsets[nameOffsets.length-1];
        topdictOffsets     = getIndex(topdictIndexOffset);
        stringIndexOffset  = topdictOffsets[topdictOffsets.length-1];
        stringOffsets      = getIndex(stringIndexOffset);
        gsubrIndexOffset   = stringOffsets[stringOffsets.length-1];
        gsubrOffsets       = getIndex(gsubrIndexOffset);
        
        fonts = new Font[nameOffsets.length-1];
        
        
        
        
        
        for (int j=0; j<nameOffsets.length-1; j++) {
            fonts[j] = new Font();
            seek(nameOffsets[j]);
            fonts[j].name = "";
            for (int k=nameOffsets[j]; k<nameOffsets[j+1]; k++) {
                fonts[j].name += getCard8();
            }
            
        }
        
        
        
        
        
        
        
        
        for (int j=0; j<topdictOffsets.length-1; j++) {
            seek(topdictOffsets[j]);
            while (getPosition() < topdictOffsets[j+1]) {                
                getDictItem();
                if (key=="FullName") {
                    
                    fonts[j].fullName = getString((char)((Integer)args[0]).intValue());
                    
                } else if (key=="ROS")
                    fonts[j].isCID = true;
                else if (key=="Private") {
                    fonts[j].privateLength  = ((Integer)args[0]).intValue();
                    fonts[j].privateOffset  = ((Integer)args[1]).intValue();
                }
                else if (key=="charset"){
                    fonts[j].charsetOffset = ((Integer)args[0]).intValue();
                    
                }
                else if (key=="Encoding"){
                    fonts[j].encodingOffset = ((Integer)args[0]).intValue();
                    ReadEncoding(fonts[j].encodingOffset);
                }
                else if (key=="CharStrings") {
                    fonts[j].charstringsOffset = ((Integer)args[0]).intValue();
                    
                    
                    int p = getPosition();
                    fonts[j].charstringsOffsets = getIndex(fonts[j].charstringsOffset);
                    seek(p);
                } else if (key=="FDArray")
                    fonts[j].fdarrayOffset = ((Integer)args[0]).intValue();
                else if (key=="FDSelect")
                    fonts[j].fdselectOffset = ((Integer)args[0]).intValue();
                else if (key=="CharstringType")
                    fonts[j].CharstringType = ((Integer)args[0]).intValue();
            }
            
            
            if (fonts[j].privateOffset >= 0) {
                
                seek(fonts[j].privateOffset);
                while (getPosition() < fonts[j].privateOffset+fonts[j].privateLength) {
                    getDictItem();
                    if (key=="Subrs")
                        
                        
                        fonts[j].privateSubrs = ((Integer)args[0]).intValue()+fonts[j].privateOffset;
                }
            }
            
            
            if (fonts[j].fdarrayOffset >= 0) {
                int[] fdarrayOffsets = getIndex(fonts[j].fdarrayOffset);
                
                fonts[j].fdprivateOffsets = new int[fdarrayOffsets.length-1];
                fonts[j].fdprivateLengths = new int[fdarrayOffsets.length-1];
                
                
                
                for (int k=0; k<fdarrayOffsets.length-1; k++) {
                    seek(fdarrayOffsets[k]);
                    while (getPosition() < fdarrayOffsets[k+1])
                        getDictItem();
                    if (key=="Private") {
                        fonts[j].fdprivateLengths[k]  = ((Integer)args[0]).intValue();
                        fonts[j].fdprivateOffsets[k]  = ((Integer)args[1]).intValue();
                    }
                    
                }
            }
        }
        
    }
    
    
    
    void ReadEncoding(int nextIndexOffset){
        int format;
        seek(nextIndexOffset);
        format = getCard8();
    }    
}