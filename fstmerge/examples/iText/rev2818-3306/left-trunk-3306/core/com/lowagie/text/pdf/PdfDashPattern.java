

package com.lowagie.text.pdf;

import java.io.IOException;
import java.io.OutputStream;



public class PdfDashPattern extends PdfArray {
    
    
    

    private float dash = -1;
    

    private float gap = -1;
    

    private float phase = -1;
    
    
    

    
    public PdfDashPattern() {
        super();
    }
    

    
    public PdfDashPattern(float dash) {
        super(new PdfNumber(dash));
        this.dash = dash;
    }
    

    
    public PdfDashPattern(float dash, float gap) {
        super(new PdfNumber(dash));
        add(new PdfNumber(gap));
        this.dash = dash;
        this.gap = gap;
    }
    

    
    public PdfDashPattern(float dash, float gap, float phase) {
        super(new PdfNumber(dash));
        add(new PdfNumber(gap));
        this.dash = dash;
        this.gap = gap;
        this.phase = phase;
    }
    
    public void add(float n) {
        add(new PdfNumber(n));
    }
    

    
    public void toPdf(PdfWriter writer, OutputStream os) throws IOException {
        os.write('[');

        if (dash >= 0) {
            new PdfNumber(dash).toPdf(writer, os);
            if (gap >= 0) {
                os.write(' ');
                new PdfNumber(gap).toPdf(writer, os);
            }
        }
        os.write(']');
        if (phase >=0) {
            os.write(' ');
            new PdfNumber(phase).toPdf(writer, os);
        }
    }
}