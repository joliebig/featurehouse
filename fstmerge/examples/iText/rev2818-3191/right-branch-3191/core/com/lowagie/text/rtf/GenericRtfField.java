

package com.lowagie.text.rtf;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Font;



public class GenericRtfField extends AbstractRtfField {
    
    protected String fieldInst;

    
    protected String fieldResult;

    
    public GenericRtfField(final String fieldInst, final String fieldResult) {
        super("x", new Font());
        this.fieldInst = fieldInst;
        this.fieldResult = fieldResult;
    }

    
    public GenericRtfField(final String fieldInst, final String fieldResult, Font font) {
        super("x", font);
        this.fieldInst = fieldInst;
        this.fieldResult = fieldResult;
    }

    
    public void writeRtfFieldInitializationStuff(OutputStream out) throws IOException {
        out.write(fieldInst.trim().getBytes());
        out.write(RtfWriter.delimiter);
    }

    
    public void writeRtfFieldResultStuff(OutputStream out) throws IOException {
        if (null != fieldResult) {
            out.write(fieldResult.trim().getBytes());
        }
    }
}
