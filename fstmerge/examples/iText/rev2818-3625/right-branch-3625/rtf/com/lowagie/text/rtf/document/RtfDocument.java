

package com.lowagie.text.rtf.document;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;

import com.lowagie.text.rtf.RtfBasicElement;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.RtfMapper;
import com.lowagie.text.rtf.document.output.RtfDataCache;
import com.lowagie.text.rtf.document.output.RtfDiskCache;
import com.lowagie.text.rtf.document.output.RtfEfficientMemoryCache;
import com.lowagie.text.rtf.document.output.RtfMemoryCache;
import com.lowagie.text.rtf.graphic.RtfImage;


public class RtfDocument extends RtfElement {
    
    private RtfDataCache data = null;
    
    private RtfMapper mapper = null;
    
    private RtfDocumentHeader documentHeader = null;
    
    private ArrayList<Integer> previousRandomInts = null;
    
    private boolean autogenerateTOCEntries = false;
    
    private RtfDocumentSettings documentSettings = null;
    
    private RtfBasicElement lastElementWritten = null;
    
    
    private static final byte[] RTF_DOCUMENT = "\\rtf1".getBytes();

    private final static byte[] FSC_LINE = "\\line ".getBytes();
    private final static byte[] FSC_PAR = "\\par ".getBytes();
    private final static byte[] FSC_TAB = "\\tab ".getBytes();
    private final static byte[] FSC_PAGE_PAR = "\\page\\par ".getBytes();
    private final static byte[] FSC_NEWPAGE = "$newpage$".getBytes();
    private final static byte[] FSC_BACKSLASH = "\\".getBytes();
    private final static byte[] FSC_HEX_PREFIX = "\\\'".getBytes();
    private final static byte[] FSC_UNI_PREFIX = "\\u".getBytes();
    
    
    public RtfDocument() {
        super(null);
        this.data = new RtfMemoryCache();
        this.mapper = new RtfMapper(this);
        this.documentHeader = new RtfDocumentHeader(this);
        this.documentHeader.init();
        this.previousRandomInts = new ArrayList<Integer>();
        this.documentSettings = new RtfDocumentSettings(this);
    }

    
    public void writeContent(final OutputStream out) throws IOException
    {        
    }
    
    
    public void writeDocument(OutputStream out) {
        try {
            out.write(OPEN_GROUP);
            out.write(RtfDocument.RTF_DOCUMENT);
            this.documentHeader.writeContent(out);
            this.data.writeTo(out);
            out.write(CLOSE_GROUP);
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
    }
    
    
    public void open() {
        try {
            switch(this.documentSettings.getDataCacheStyle()) {
                case RtfDataCache.CACHE_MEMORY_EFFICIENT:  
                    this.data = new RtfEfficientMemoryCache(); 
                    break;
                case RtfDataCache.CACHE_MEMORY:
                    this.data = new RtfMemoryCache();
                    break;
                case RtfDataCache.CACHE_DISK:
                    this.data = new RtfDiskCache();
                    break;
                default:
                    throw new RuntimeException("unknown");
            }
            
        } catch(IOException ioe) {
            System.err.println("Could not initialize disk cache. Using memory cache.");
            ioe.printStackTrace();
            this.data = new RtfMemoryCache();
        }
    }
    
    
    public void add(RtfBasicElement element) {
        try {
            if(element instanceof RtfInfoElement) {
                this.documentHeader.addInfoElement((RtfInfoElement) element);
            } else {
                if(element instanceof RtfImage) {
                    ((RtfImage) element).setTopLevelElement(true);
                }
                element.writeContent( this.data.getOutputStream() );
                this.lastElementWritten = element;
            }
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
    }
    
    
    public RtfMapper getMapper() {
        return this.mapper;
    }
    
    
    public int getRandomInt() {
        Integer newInt = null;
        do {

                newInt = new Integer((int) (Math.random() * Integer.MAX_VALUE));

        } while(this.previousRandomInts.contains(newInt));
        this.previousRandomInts.add(newInt);
        return newInt.intValue();
    }
    
    
    public RtfDocumentHeader getDocumentHeader() {
        return this.documentHeader;
    }
    
    
    public void filterSpecialChar(final OutputStream out, final String str, final boolean useHex, final boolean softLineBreaks) throws IOException
    {
        if(out == null) {
            throw new NullPointerException("null OutpuStream");
        }

        final boolean alwaysUseUniCode = this.documentSettings.isAlwaysUseUnicode();
        if(str == null) {
            return;
        }
        final int len = str.length();
        if(len == 0) {
            return;
        }

        for(int k = 0; k < len; k++) {
            final char c = str.charAt(k);
            if(c < 0x20) {
                
                if(c == '\n') {
                    out.write(softLineBreaks ? FSC_LINE : FSC_PAR);
                } else if(c == '\t') {
                    out.write(FSC_TAB);                 
                } else {
                    out.write('?');
                }
            } else if((c == '\\') || (c == '{') || (c == '}')) {
                
                out.write(FSC_BACKSLASH);
                out.write(c);
            } else if((c == '$') && (len-k >= FSC_NEWPAGE.length) && subMatch(str, k, FSC_NEWPAGE)) {
                out.write(FSC_PAGE_PAR);
                k += FSC_NEWPAGE.length-1;
            } else {
                if((c > 0xff) || ((c > 'z') && alwaysUseUniCode)) {
                    if(useHex && (c <= 0xff)) {
                        
                        out.write(FSC_HEX_PREFIX);
                        out.write(RtfImage.byte2charLUT, c*2, 2);
                    } else {
                        
                        out.write(FSC_UNI_PREFIX);
                        String s = Short.toString((short)c);
                        for(int x = 0; x < s.length(); x++) {
                            out.write(s.charAt(x));
                        }
                        out.write('?');
                    }
                } else {
                    out.write(c);
                }
            }
        }       
    }
    
    private static boolean subMatch(final String str, int soff, final byte[] m)
    {
        for(int k = 0; k < m.length; k++) {
            if(str.charAt(soff++) != m[k]) {
                return false;
            }
        }
        return true;
    }
    
    
    public void setAutogenerateTOCEntries(boolean autogenerate) {
        this.autogenerateTOCEntries = autogenerate;
    }
    
    
    public boolean getAutogenerateTOCEntries() {
        return this.autogenerateTOCEntries;
    }
    
    
    public RtfDocumentSettings getDocumentSettings() {
        return this.documentSettings;
    }
    
    
    public RtfBasicElement getLastElementWritten() {
        return this.lastElementWritten;
    }
    
    
    final public void outputDebugLinebreak(final OutputStream result) throws IOException {
        if(this.getDocumentSettings().isOutputDebugLineBreaks())
        {
            result.write('\n');
        }
    }
}