

package com.lowagie.text.rtf;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Font;


public class RtfPageNumber extends GenericRtfField {
    private String content;

    
    public RtfPageNumber( String content, Font contentFont ) {
        super("PAGE", "", contentFont);
        this.content = content;
    }

    
    public void write( RtfWriter writer, OutputStream out ) throws IOException {
        writer.writeInitialFontSignature( out, this );
        out.write(content.getBytes());
        writer.writeFinishingFontSignature( out, this );
        super.write(writer, out);
    }

    
    public String toString() {
        return content;
    }
}
