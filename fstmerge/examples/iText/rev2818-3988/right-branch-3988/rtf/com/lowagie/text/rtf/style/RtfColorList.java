

package com.lowagie.text.rtf.style;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;

import com.lowagie.text.DocWriter;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.RtfExtendedElement;
import com.lowagie.text.rtf.document.RtfDocument;



public class RtfColorList extends RtfElement implements RtfExtendedElement {

    
    private static final byte[] COLOR_TABLE = DocWriter.getISOBytes("\\colortbl");
    
    
    ArrayList<RtfColor> colorList = new ArrayList<RtfColor>();
    
    
    public RtfColorList(RtfDocument doc) {
        super(doc);
        colorList.add(new RtfColor(doc, 0, 0, 0, 0));
        colorList.add(new RtfColor(doc, 255, 255, 255, 1));
    }
    
    
    public int getColorNumber(RtfColor color) {
        int colorIndex = -1;
        for(int i = 0; i < colorList.size(); i++) {
            if(colorList.get(i).equals(color)) {
                colorIndex = i;
            }
        }
        if(colorIndex == -1) {
            colorIndex = colorList.size();
            colorList.add(color);
        }
        return colorIndex;
    }
    
    
    public void writeContent(final OutputStream out) throws IOException
    {        
    }
    
    
    public void writeDefinition(final OutputStream result) throws IOException
    {
        result.write(OPEN_GROUP);
        result.write(COLOR_TABLE);
        for(int i = 0; i < colorList.size(); i++) {
            RtfColor color = colorList.get(i);
            color.writeDefinition(result);
        }
        result.write(CLOSE_GROUP);
        this.document.outputDebugLinebreak(result);        
    }
    

}
