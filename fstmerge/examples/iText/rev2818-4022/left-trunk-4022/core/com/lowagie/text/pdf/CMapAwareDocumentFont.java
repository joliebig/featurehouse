
package com.lowagie.text.pdf;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import com.lowagie.text.pdf.fonts.cmaps.CMap;
import com.lowagie.text.pdf.fonts.cmaps.CMapParser;



public class CMapAwareDocumentFont extends DocumentFont {

    
    private PdfDictionary fontDic;
    
    private int spaceWidth;
    
    private CMap toUnicodeCmap;
    
    private char[] cidbyte2uni;
    
    
    public CMapAwareDocumentFont(PRIndirectReference refFont) {
        super(refFont);
        fontDic = (PdfDictionary)PdfReader.getPdfObjectRelease(refFont);

        processToUnicode();
        if (toUnicodeCmap == null)
            processUni2Byte();
        
        spaceWidth = super.getWidth(' ');
        if (spaceWidth == 0){
            spaceWidth = computeAverageWidth();
        }
        
    }

    
    private void processToUnicode(){
        
        PdfObject toUni = fontDic.get(PdfName.TOUNICODE);
        if (toUni != null){
            
            try {
                byte[] touni = PdfReader.getStreamBytes((PRStream)PdfReader.getPdfObjectRelease(toUni));
    
                CMapParser cmapParser = new CMapParser();
                toUnicodeCmap = cmapParser.parse(new ByteArrayInputStream(touni));
            } catch (IOException e) {
                throw new Error("Unable to process ToUnicode map - " + e.getMessage(), e);
            }
        }
    }
    
    
    private void processUni2Byte(){
        IntHashtable uni2byte = getUni2Byte();
        int e[] = uni2byte.toOrderedKeys();
        
        cidbyte2uni = new char[256];
        for (int k = 0; k < e.length; ++k) {
            int n = uni2byte.get(e[k]);
            
            
            
            
            if (cidbyte2uni[n] == 0)
                cidbyte2uni[n] = (char)e[k];
        }
    }
    

    
    
    private int computeAverageWidth(){
        int count = 0;
        int total = 0;
        for(int i = 0; i < super.widths.length; i++){
            if(super.widths[i] != 0){
                total += super.widths[i];
                count++;
            }
        }
        return count != 0 ? total/count : 0;
    }
    
    
    public int getWidth(int char1) {
        if (char1 == ' ')
            return spaceWidth;
        
        return super.getWidth(char1);
    }
    
    
    private String decodeSingleCID(byte[] bytes, int offset, int len){
        if (toUnicodeCmap != null){
            if (offset + len > bytes.length)
                throw new ArrayIndexOutOfBoundsException("Invalid index: " + offset + len);
            return toUnicodeCmap.lookup(bytes, offset, len);
        }

        if (len == 1){
            return new String(cidbyte2uni, 0xff & bytes[offset], 1);
        }
        
        throw new Error("Multi-byte glyphs not implemented yet");
    }

    
    public String decode(byte[] cidbytes, final int offset, final int len){
        StringBuffer sb = new StringBuffer(); 
        for(int i = offset; i < offset + len; i++){
            String rslt = decodeSingleCID(cidbytes, i, 1);
            if (rslt == null){
                rslt = decodeSingleCID(cidbytes, i, 2);
                i++;
            }
            sb.append(rslt);
        }

        return sb.toString();
    }

    
    public String encode(byte[] bytes, int offset, int len){
        return decode(bytes, offset, len);    
    }
}
