package com.lowagie.text.pdf.collection;

import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfString;

public class PdfCollection extends PdfDictionary {

    
    public static final int DETAILS = 0;
    
    public static final int TILE = 1;
    
    public static final int HIDDEN = 2;
    
    
    public PdfCollection(int type) {
        super(PdfName.COLLECTION);
        switch(type) {
        case TILE:
            put(PdfName.VIEW, PdfName.T);
            break;
        case HIDDEN:
            put(PdfName.VIEW, PdfName.H);
            break;
        default:
            put(PdfName.VIEW, PdfName.D);
        }
    }
    
    
    public void setInitialDocument(String description) {
        put(PdfName.D, new PdfString(description, null));
    }
    
    
    public void setSchema(PdfCollectionSchema schema) {
        put(PdfName.SCHEMA, schema);
    }
    
    
    public PdfCollectionSchema getSchema() {
        return (PdfCollectionSchema)get(PdfName.SCHEMA);
    }
    
    
    public void setSort(PdfCollectionSort sort) {
        put(PdfName.SORT, sort);
    }
}