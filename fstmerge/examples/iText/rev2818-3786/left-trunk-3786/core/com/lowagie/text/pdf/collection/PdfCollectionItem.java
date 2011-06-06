package com.lowagie.text.pdf.collection;

import java.util.Calendar;

import com.lowagie.text.pdf.PdfDate;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfString;

public class PdfCollectionItem extends PdfDictionary {
    
    
    PdfCollectionSchema schema;
    
    
    public PdfCollectionItem(PdfCollectionSchema schema) {
        super(PdfName.COLLECTIONITEM);
        this.schema = schema;
    }
    
    
    public void addItem(String key, String value) {
        PdfName fieldname = new PdfName(key);
        PdfCollectionField field = (PdfCollectionField)schema.get(fieldname);
        put(fieldname, field.getValue(value));
    }
    
    
    public void addItem(String key, PdfString value) {
        PdfName fieldname = new PdfName(key);
        PdfCollectionField field = (PdfCollectionField)schema.get(fieldname);
        if (field.fieldType == PdfCollectionField.TEXT) {
            put(fieldname, value);
        }
    }
    
    
    public void addItem(String key, PdfDate d) {
        PdfName fieldname = new PdfName(key);
        PdfCollectionField field = (PdfCollectionField)schema.get(fieldname);
        if (field.fieldType == PdfCollectionField.DATE) {
            put(fieldname, d);
        }
    }
    
    
    public void addItem(String key, PdfNumber n) {
        PdfName fieldname = new PdfName(key);
        PdfCollectionField field = (PdfCollectionField)schema.get(fieldname);
        if (field.fieldType == PdfCollectionField.NUMBER) {
            put(fieldname, n);
        }
    }
    
    
    public void addItem(String key, Calendar c) {
        addItem(key, new PdfDate(c));
    }
    
    
    public void addItem(String key, int i) {
        addItem(key, new PdfNumber(i));
    }
    
    
    public void addItem(String key, float f) {
        addItem(key, new PdfNumber(f));
    }
    
    
    public void addItem(String key, double d) {
        addItem(key, new PdfNumber(d));
    }
    
    
    public void setPrefix(String key, String prefix) {
        PdfName fieldname = new PdfName(key);
        PdfObject o = get(fieldname);
        if (o == null)
            throw new IllegalArgumentException("You must set a value before adding a prefix.");
        PdfDictionary dict = new PdfDictionary(PdfName.COLLECTIONSUBITEM);
        dict.put(PdfName.D, o);
        dict.put(PdfName.P, new PdfString(prefix, PdfObject.TEXT_UNICODE));
        put(fieldname, dict);
    }
}