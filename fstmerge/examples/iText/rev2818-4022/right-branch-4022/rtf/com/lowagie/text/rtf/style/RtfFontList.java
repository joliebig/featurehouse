

package com.lowagie.text.rtf.style;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;

import com.lowagie.text.DocWriter;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.RtfExtendedElement;
import com.lowagie.text.rtf.document.RtfDocument;


public class RtfFontList extends RtfElement implements RtfExtendedElement {
    
    
    private static final byte[] DEFAULT_FONT = DocWriter.getISOBytes("\\deff");
    
    private static final byte[] FONT_TABLE = DocWriter.getISOBytes("\\fonttbl");
    
    public static final byte[] FONT_NUMBER = DocWriter.getISOBytes("\\f");
    
    
    private ArrayList<RtfFont> fontList = new ArrayList<RtfFont>();

    
    public RtfFontList(RtfDocument doc) {
        super(doc);
        fontList.add(new RtfFont(document, 0));
    }

    
    public void writeContent(OutputStream out) throws IOException
    {    
    }
    
    
    public int getFontNumber(RtfFont font) {
        if(font instanceof RtfParagraphStyle) {
            font = new RtfFont(this.document, font);
        }
        int fontIndex = -1;
        for(int i = 0; i < fontList.size(); i++) {
            if(fontList.get(i).equals(font)) {
                fontIndex = i;
            }
        }
        if(fontIndex == -1) {
            fontIndex = fontList.size();
            fontList.add(font);
        }
        return fontIndex;
    }

    
    public void writeDefinition(final OutputStream result) throws IOException 
    {
        result.write(DEFAULT_FONT);
        result.write(intToByteArray(0));
        result.write(OPEN_GROUP);
        result.write(FONT_TABLE);
        for(int i = 0; i < fontList.size(); i++) {
            result.write(OPEN_GROUP);
            result.write(FONT_NUMBER);
            result.write(intToByteArray(i));
            RtfFont rf = fontList.get(i);
            rf.writeDefinition(result);
            result.write(COMMA_DELIMITER);
            result.write(CLOSE_GROUP);
        }
        result.write(CLOSE_GROUP);
        this.document.outputDebugLinebreak(result);        
    }
    
}
