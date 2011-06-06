

package com.lowagie.text.rtf.text;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.rtf.RtfAddableElement;


public class RtfTab extends RtfAddableElement {

    
    public static final int TAB_LEFT_ALIGN = 0;
    
    public static final int TAB_CENTER_ALIGN = 1;
    
    public static final int TAB_RIGHT_ALIGN = 2;
    
    public static final int TAB_DECIMAL_ALIGN = 3;
    
    
    private int position = 0;
    
    private int type = TAB_LEFT_ALIGN;
    
    
    public RtfTab(float position, int type) {
        this.position = (int) Math.round(position * TWIPS_FACTOR);
        switch(type) {
        case TAB_LEFT_ALIGN: this.type = TAB_LEFT_ALIGN; break;
        case TAB_CENTER_ALIGN: this.type = TAB_CENTER_ALIGN; break;
        case TAB_RIGHT_ALIGN: this.type = TAB_RIGHT_ALIGN; break;
        case TAB_DECIMAL_ALIGN: this.type = TAB_DECIMAL_ALIGN; break;
        default: this.type = TAB_LEFT_ALIGN; break;
        }
    }
    
    
    public void writeContent(final OutputStream result) throws IOException
    {
        switch(this.type) {
            case TAB_CENTER_ALIGN: result.write("\\tqc".getBytes()); break;
            case TAB_RIGHT_ALIGN: result.write("\\tqr".getBytes()); break;
            case TAB_DECIMAL_ALIGN: result.write("\\tqdec".getBytes()); break;
        }
        result.write("\\tx".getBytes());
        result.write(intToByteArray(this.position));        
    }
    
}
