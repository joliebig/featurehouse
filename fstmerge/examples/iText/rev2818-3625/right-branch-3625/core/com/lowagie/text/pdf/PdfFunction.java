
package com.lowagie.text.pdf;

import java.io.IOException;

import com.lowagie.text.ExceptionConverter;

public class PdfFunction {
    
    protected PdfWriter writer;
    
    protected PdfIndirectReference reference;
    
    protected PdfDictionary dictionary;
    
    
    protected PdfFunction(PdfWriter writer) {
        this.writer = writer;
    }
    
    PdfIndirectReference getReference() {
        try {
            if (reference == null) {
                reference = writer.addToBody(dictionary).getIndirectReference();
            }
        }
        catch (IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
        return reference;
    }
        
    public static PdfFunction type0(PdfWriter writer, float domain[], float range[], int size[],
        int bitsPerSample, int order, float encode[], float decode[], byte stream[]) {
        PdfFunction func = new PdfFunction(writer);
        func.dictionary = new PdfStream(stream);
        ((PdfStream)func.dictionary).flateCompress(writer.getCompressionLevel());
        func.dictionary.put(PdfName.FUNCTIONTYPE, new PdfNumber(0));
        func.dictionary.put(PdfName.DOMAIN, new PdfArray(domain));
        func.dictionary.put(PdfName.RANGE, new PdfArray(range));
        func.dictionary.put(PdfName.SIZE, new PdfArray(size));
        func.dictionary.put(PdfName.BITSPERSAMPLE, new PdfNumber(bitsPerSample));
        if (order != 1)
            func.dictionary.put(PdfName.ORDER, new PdfNumber(order));
        if (encode != null)
            func.dictionary.put(PdfName.ENCODE, new PdfArray(encode));
        if (decode != null)
            func.dictionary.put(PdfName.DECODE, new PdfArray(decode));
        return func;
    }

    public static PdfFunction type2(PdfWriter writer, float domain[], float range[], float c0[], float c1[], float n) {
        PdfFunction func = new PdfFunction(writer);
        func.dictionary = new PdfDictionary();
        func.dictionary.put(PdfName.FUNCTIONTYPE, new PdfNumber(2));
        func.dictionary.put(PdfName.DOMAIN, new PdfArray(domain));
        if (range != null)
            func.dictionary.put(PdfName.RANGE, new PdfArray(range));
        if (c0 != null)
            func.dictionary.put(PdfName.C0, new PdfArray(c0));
        if (c1 != null)
            func.dictionary.put(PdfName.C1, new PdfArray(c1));
        func.dictionary.put(PdfName.N, new PdfNumber(n));
        return func;
    }

    public static PdfFunction type3(PdfWriter writer, float domain[], float range[], PdfFunction functions[], float bounds[], float encode[]) {
        PdfFunction func = new PdfFunction(writer);
        func.dictionary = new PdfDictionary();
        func.dictionary.put(PdfName.FUNCTIONTYPE, new PdfNumber(3));
        func.dictionary.put(PdfName.DOMAIN, new PdfArray(domain));
        if (range != null)
            func.dictionary.put(PdfName.RANGE, new PdfArray(range));
        PdfArray array = new PdfArray();
        for (int k = 0; k < functions.length; ++k)
            array.add(functions[k].getReference());
        func.dictionary.put(PdfName.FUNCTIONS, array);
        func.dictionary.put(PdfName.BOUNDS, new PdfArray(bounds));
        func.dictionary.put(PdfName.ENCODE, new PdfArray(encode));
        return func;
    }
    
    public static PdfFunction type4(PdfWriter writer, float domain[], float range[], String postscript) {
        byte b[] = new byte[postscript.length()];
        for (int k = 0; k < b.length; ++k)
            b[k] = (byte)postscript.charAt(k);
        PdfFunction func = new PdfFunction(writer);
        func.dictionary = new PdfStream(b);
        ((PdfStream)func.dictionary).flateCompress(writer.getCompressionLevel());
        func.dictionary.put(PdfName.FUNCTIONTYPE, new PdfNumber(4));
        func.dictionary.put(PdfName.DOMAIN, new PdfArray(domain));
        func.dictionary.put(PdfName.RANGE, new PdfArray(range));
        return func;
    }
}
