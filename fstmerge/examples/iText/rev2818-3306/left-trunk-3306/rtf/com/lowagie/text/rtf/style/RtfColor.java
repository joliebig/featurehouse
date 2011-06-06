

package com.lowagie.text.rtf.style;

import java.awt.Color;
import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.RtfExtendedElement;
import com.lowagie.text.rtf.document.RtfDocument;



public class RtfColor extends RtfElement implements RtfExtendedElement {

    
    private static final byte[] COLOR_RED = "\\red".getBytes();
    
    private static final byte[] COLOR_GREEN = "\\green".getBytes();
    
    private static final byte[] COLOR_BLUE = "\\blue".getBytes();
    
    private static final byte COLON = (byte) ';';
    
    private static final byte[] COLOR_NUMBER = "\\cf".getBytes();

    
    private int colorNumber = 0;
    
    private int red = 0;
    
    private int green = 0;
    
    private int blue = 0;
    
    
    protected RtfColor(RtfDocument doc, int red, int green, int blue, int colorNumber) {
        super(doc);
        this.red = red;
        this.blue = blue;
        this.green = green;
        this.colorNumber = colorNumber;
    }
    
    
    public RtfColor(RtfDocument doc, RtfColor col) {
        super(doc);
        if(col != null) {
            this.red = col.getRed();
            this.green = col.getGreen();
            this.blue = col.getBlue();
        }
        if(this.document != null) {
            this.colorNumber = this.document.getDocumentHeader().getColorNumber(this);
        }
    }
    
    
    public RtfColor(RtfDocument doc, Color col) {
        super(doc);
        if(col != null) {
            this.red = col.getRed();
            this.blue = col.getBlue();
            this.green = col.getGreen();
        }
        if(this.document != null) {
            this.colorNumber = this.document.getDocumentHeader().getColorNumber(this);
        }
    }
    
    
    public RtfColor(RtfDocument doc, int red, int green, int blue) {
        super(doc);
        this.red = red;
        this.blue = blue;
        this.green = green;
        if(this.document != null) {
            this.colorNumber = this.document.getDocumentHeader().getColorNumber(this);
        }
    }

    
    public void writeContent(final OutputStream out) throws IOException
    {        
    }
    
    
    public void writeDefinition(final OutputStream result) throws IOException
    {
        result.write(COLOR_RED);
        result.write(intToByteArray(red));
        result.write(COLOR_GREEN);
        result.write(intToByteArray(green));
        result.write(COLOR_BLUE);
        result.write(intToByteArray(blue));
        result.write(COLON);        
    }

    
    public void writeBegin(final OutputStream result) {
        try {
            result.write(COLOR_NUMBER);
            result.write(intToByteArray(colorNumber));
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
    }
    
    
    public void writeEnd(final OutputStream result) {
    }
    
    
    public boolean equals(Object obj) {
        if(!(obj instanceof RtfColor)) {
            return false;
        }
        RtfColor color = (RtfColor) obj;
        return (this.red == color.getRed() && this.green == color.getGreen() && this.blue == color.getBlue());
    }

    
    public int hashCode() {
        return (this.red << 16) | (this.green << 8) | this.blue;
    }
    
    
    public int getBlue() {
        return blue;
    }

    
    public int getGreen() {
        return green;
    }

    
    public int getRed() {
        return red;
    }
    
    
    public int getColorNumber() {
        return colorNumber;
    }

    
    public void setRtfDocument(RtfDocument doc) {
        super.setRtfDocument(doc);
        if(document != null) {
            this.colorNumber = document.getDocumentHeader().getColorNumber(this);
        }
    }
}
