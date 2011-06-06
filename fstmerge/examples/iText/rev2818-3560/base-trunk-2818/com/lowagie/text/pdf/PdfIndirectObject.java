

package com.lowagie.text.pdf;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.DocWriter;



public class PdfIndirectObject {
    
    
    

    protected int number;
    

    protected int generation = 0;
    
    static final byte STARTOBJ[] = DocWriter.getISOBytes(" obj");
    static final byte ENDOBJ[] = DocWriter.getISOBytes("\nendobj\n");
    static final int SIZEOBJ = STARTOBJ.length + ENDOBJ.length;
    PdfObject object;
    PdfWriter writer;
    
    
    

    
    PdfIndirectObject(int number, PdfObject object, PdfWriter writer) {
        this(number, 0, object, writer);
    }
    
    PdfIndirectObject(PdfIndirectReference ref, PdfObject object, PdfWriter writer) {
        this(ref.getNumber(),ref.getGeneration(),object,writer);
    }

    
    PdfIndirectObject(int number, int generation, PdfObject object, PdfWriter writer) {
        this.writer = writer;
        this.number = number;
        this.generation = generation;
        this.object = object;
        PdfEncryption crypto = null;
        if (writer != null)
            crypto = writer.getEncryption();
        if (crypto != null) {
            crypto.setHashKey(number, generation);
        }
    }
    
    
    

    






    
    

    
    public PdfIndirectReference getIndirectReference() {
        return new PdfIndirectReference(object.type(), number, generation);
    }
    

    void writeTo(OutputStream os) throws IOException
    {
        os.write(DocWriter.getISOBytes(String.valueOf(number)));
        os.write(' ');
        os.write(DocWriter.getISOBytes(String.valueOf(generation)));
        os.write(STARTOBJ);
        int type = object.type();
        if (type != PdfObject.ARRAY && type != PdfObject.DICTIONARY && type != PdfObject.NAME && type != PdfObject.STRING)
            os.write(' ');
        object.toPdf(writer, os);
        os.write(ENDOBJ);
    }
}
