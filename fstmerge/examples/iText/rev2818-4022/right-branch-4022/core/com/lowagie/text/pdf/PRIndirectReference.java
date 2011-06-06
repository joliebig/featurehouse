

package com.lowagie.text.pdf;
import java.io.IOException;
import java.io.OutputStream;

public class PRIndirectReference extends PdfIndirectReference {
    
    protected PdfReader reader;
    
    
    
    

    
    PRIndirectReference(PdfReader reader, int number, int generation) {
        type = INDIRECT;
        this.number = number;
        this.generation = generation;
        this.reader = reader;
    }
    

    
    PRIndirectReference(PdfReader reader, int number) {
        this(reader, number, 0);
    }
    
    
    
    public void toPdf(PdfWriter writer, OutputStream os) throws IOException {
        int n = writer.getNewObjectNumber(reader, number, generation);
        os.write(PdfEncodings.convertToBytes(new StringBuffer().append(n).append(" 0 R").toString(), null));
    }

    public PdfReader getReader() {
        return reader;
    }
    
    public void setNumber(int number, int generation) {
        this.number = number;
        this.generation = generation;
    }
}