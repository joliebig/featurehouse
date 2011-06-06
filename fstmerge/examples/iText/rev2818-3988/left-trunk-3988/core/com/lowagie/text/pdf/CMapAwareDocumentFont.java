
package com.lowagie.text.pdf;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import com.lowagie.text.pdf.fonts.cmaps.CMap;
import com.lowagie.text.pdf.fonts.cmaps.CMapParser;


public class CMapAwareDocumentFont extends DocumentFont {

    
    private PdfDictionary fontDic;
    
    private CMap cmap;
    
    private int spaceWidth;
    
    
    public CMapAwareDocumentFont(PRIndirectReference refFont) {
        super(refFont);
        fontDic = (PdfDictionary)PdfReader.getPdfObjectRelease(refFont);
        processToUni();
        spaceWidth = super.getWidth(' ');
        if (spaceWidth == 0){
            spaceWidth = computeAverageWidth();
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
    
    
    private void processToUni(){
        
        PdfObject toUni = fontDic.get(PdfName.TOUNICODE);

        if (toUni == null)
            return;
        
        try {
            byte[] cmapBytes = PdfReader.getStreamBytes((PRStream)PdfReader.getPdfObjectRelease(toUni));
            CMapParser cmapParser = new CMapParser();
            cmap = cmapParser.parse(new ByteArrayInputStream(cmapBytes));
        } catch (IOException e) {
            throw new Error("Unable to obtain cmap - " + e.getMessage(), e);
        }

    }
    
    
    public String encode(byte[] bytes, int offset, int len){
            if (cmap != null){
                if (len > bytes.length)
                    System.out.println("Length problem...");
                return cmap.lookup(bytes, offset, len);
            }
        
            if (len == 1)
                return new String(bytes, offset, 1);
            
            throw new Error("Multi-byte glyphs not implemented yet");
    }

}
