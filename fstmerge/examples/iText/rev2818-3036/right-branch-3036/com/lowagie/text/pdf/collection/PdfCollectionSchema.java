package com.lowagie.text.pdf.collection;

import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;

public class PdfCollectionSchema extends PdfDictionary {
    
    public PdfCollectionSchema() {
        super(PdfName.COLLECTIONSCHEMA);
    }
    
    
    public void addField(String name, PdfCollectionField field) {
        put(new PdfName(name), field);
    }
}
