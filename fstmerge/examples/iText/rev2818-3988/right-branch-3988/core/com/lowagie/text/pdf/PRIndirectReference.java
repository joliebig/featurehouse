

package com.lowagie.text.pdf;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.ref.WeakReference;

public class PRIndirectReference extends PdfIndirectReference {

    protected PdfReader reader;
    
    
    protected PdfObject hardReference;

    



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
        
        hardReference = null;
        reffedObj = null;
    }

    
    public PdfObject getDirectObject() {
        PdfObject dirObj = getInternalObject();
        if (dirObj == null) {
            dirObj = reader.getPdfObject(number);
            if (reader.isPartial()) {
                
                reffedObj = new WeakReference<PdfObject>(dirObj);
                reader.releaseLastXrefPartial();
                hardReference = null;
            }
            else {
                reffedObj = null;
                hardReference = dirObj;
            }
        }

        return dirObj;
    }

    
    private PdfObject getInternalObject() {
        if (hardReference == null && (reffedObj == null || reffedObj.get() == null)) {
            return null;
        }

        return (hardReference != null) ? hardReference : (PdfObject) reffedObj.get();
    }

    
    public void setDirectObject( PdfObject obj) {
    }
}