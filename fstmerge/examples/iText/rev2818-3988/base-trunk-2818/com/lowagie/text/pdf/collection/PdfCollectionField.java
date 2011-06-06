package com.lowagie.text.pdf.collection;

import com.lowagie.text.pdf.PdfBoolean;
import com.lowagie.text.pdf.PdfDate;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfString;


public class PdfCollectionField extends PdfDictionary {
    
    public static final int TEXT = 0;
    
    public static final int DATE = 1;
    
    public static final int NUMBER = 2;
    
    public static final int FILENAME = 3;
    
    public static final int DESC = 4;
    
    public static final int MODDATE = 5;
    
    public static final int CREATIONDATE = 6;
    
    public static final int SIZE = 7;
    
    
    protected int type;

    
    public PdfCollectionField(String name, int type) {
        super(PdfName.COLLECTIONFIELD);
        put(PdfName.N, new PdfString(name, PdfObject.TEXT_UNICODE));
        this.type = type;
        switch(type) {
        default:
            put(PdfName.SUBTYPE, PdfName.S);
            break;
        case DATE:
            put(PdfName.SUBTYPE, PdfName.D);
            break;
        case NUMBER:
            put(PdfName.SUBTYPE, PdfName.N);
            break;
        case FILENAME:
            put(PdfName.SUBTYPE, PdfName.F);
            break;
        case DESC:
            put(PdfName.SUBTYPE, PdfName.DESC);
            break;
        case MODDATE:
            put(PdfName.SUBTYPE, PdfName.MODDATE);
            break;
        case CREATIONDATE:
            put(PdfName.SUBTYPE, PdfName.CREATIONDATE);
            break;
        case SIZE:
            put(PdfName.SUBTYPE, PdfName.SIZE);
            break;
        }
    }
    
    
    public void setOrder(int i) {
        put(PdfName.O, new PdfNumber(i));
    }
    
    
    public void setVisible(boolean visible) {
        put(PdfName.V, new PdfBoolean(visible));
    }
    
    
    public void setEditable(boolean editable) {
        put(PdfName.E, new PdfBoolean(editable));
    }

    
    public boolean isCollectionItem() {
        switch(type) {
        case TEXT:
        case DATE:
        case NUMBER:
            return true;
        default:
            return false;
        }
    }
    
    
    public PdfObject getValue(String v) {
        switch(type) {
        case TEXT:
            return new PdfString(v, PdfObject.TEXT_UNICODE);
        case DATE:
            return new PdfDate(PdfDate.decode(v));
        case NUMBER:
            return new PdfNumber(v);
        }
        throw new IllegalArgumentException(v + " is not an acceptable value for the field " + get(PdfName.N).toString());
    }
}