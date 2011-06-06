package com.lowagie.text.pdf.collection;

import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfBoolean;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfObject;

public class PdfCollectionSort extends PdfDictionary {
    
    
    public PdfCollectionSort(String key) {
        super(PdfName.COLLECTIONSORT);
        put(PdfName.S, new PdfName(key));
    }
    
    
    public PdfCollectionSort(String[] keys) {
        super(PdfName.COLLECTIONSORT);
        PdfArray array = new PdfArray();
        for (int i = 0; i < keys.length; i++) {
            array.add(new PdfName(keys[i]));
        }
        put(PdfName.S, array);
    }
    
    
    public void setSortOrder(boolean ascending) {
        PdfObject o = get(PdfName.S);
        if (o instanceof PdfName) {
            put(PdfName.A, new PdfBoolean(ascending));
        }
        else {
            throw new IllegalArgumentException("You have to define a boolean array for this collection sort dictionary.");
        }
    }
    
    
    public void setSortOrder(boolean[] ascending) {
        PdfObject o = get(PdfName.S);
        if (o instanceof PdfArray) {
            if (((PdfArray)o).size() != ascending.length) {
                throw new IllegalArgumentException("The number of booleans in this array doesn't correspond with the number of fields.");
            }
            PdfArray array = new PdfArray();
            for (int i = 0; i < ascending.length; i++) {
                array.add(new PdfBoolean(ascending[i]));
            }
            put(PdfName.A, array);
        }
        else {
            throw new IllegalArgumentException("You need a single boolean for this collection sort dictionary.");
        }
    }
    
    
}
