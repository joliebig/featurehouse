

package com.lowagie.text.pdf;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.ListIterator;


public class PdfArray extends PdfObject {

    

    
    protected ArrayList<PdfObject> arrayList;

    

    
    public PdfArray() {
        super(ARRAY);
        arrayList = new ArrayList<PdfObject>();
    }

    
    public PdfArray(PdfObject object) {
        super(ARRAY);
        arrayList = new ArrayList<PdfObject>();
        arrayList.add(object);
    }

    
    public PdfArray(float values[]) {
        super(ARRAY);
        arrayList = new ArrayList<PdfObject>();
        add(values);
    }
    
    
    public PdfArray(int values[]) {
        super(ARRAY);
        arrayList = new ArrayList<PdfObject>();
        add(values);
    }

    
    public PdfArray(ArrayList<PdfObject> l) {
        this();
        for (PdfObject o: l)
            add(o);
    }

    
    public PdfArray(PdfArray array) {
        super(ARRAY);
        arrayList = new ArrayList<PdfObject>(array.arrayList);
    }

    

    
    public void toPdf(PdfWriter writer, OutputStream os) throws IOException {
        os.write('[');

        Iterator<PdfObject> i = arrayList.iterator();
        PdfObject object;
        int type = 0;
        if (i.hasNext()) {
            object = i.next();
            if (object == null)
                object = PdfNull.PDFNULL;
            object.toPdf(writer, os);
        }
        while (i.hasNext()) {
            object = i.next();
            if (object == null)
                object = PdfNull.PDFNULL;
            type = object.type();
            if (type != PdfObject.ARRAY && type != PdfObject.DICTIONARY && type != PdfObject.NAME && type != PdfObject.STRING)
                os.write(' ');
            object.toPdf(writer, os);
        }
        os.write(']');
    }

    
    public String toString() {
        return arrayList.toString();
    }
    
    
    
    
    public PdfObject set(int idx, PdfObject obj) {
        return arrayList.set(idx, obj);
    }

    
    public PdfObject remove(int idx) {
        return arrayList.remove(idx);
    }

    
    public ArrayList<PdfObject> getArrayList() {
        return arrayList;
    }

    
    public int size() {
        return arrayList.size();
    }

    
    public boolean isEmpty() {
        return arrayList.isEmpty();
    }

    
    public boolean add(PdfObject object) {
        return arrayList.add(object);
    }

    
    public boolean add(float values[]) {
        for (int k = 0; k < values.length; ++k)
            arrayList.add(new PdfNumber(values[k]));
        return true;
    }

    
    public boolean add(int values[]) {
        for (int k = 0; k < values.length; ++k)
            arrayList.add(new PdfNumber(values[k]));
        return true;
    }

    
    public void add(int index, PdfObject element) {
        arrayList.add(index, element);
    }

    
    public void addFirst(PdfObject object) {
        arrayList.add(0, object);
    }

    
    public boolean contains(PdfObject object) {
        return arrayList.contains(object);
    }

    
    public ListIterator<PdfObject> listIterator() {
        return arrayList.listIterator();
    }

    
    public PdfObject getPdfObject(int idx) {
        return arrayList.get(idx);
    }

    
    public PdfObject getDirectObject(int idx) {
        return PdfReader.getPdfObject(getPdfObject(idx));
    }

    
    
    
    
    public PdfDictionary getAsDict(int idx) {
        PdfDictionary dict = null;
        PdfObject orig = getDirectObject(idx);
        if (orig != null && orig.isDictionary())
            dict = (PdfDictionary) orig;
        return dict;
    }

    
    public PdfArray getAsArray(int idx) {
        PdfArray array = null;
        PdfObject orig = getDirectObject(idx);
        if (orig != null && orig.isArray())
            array = (PdfArray) orig;
        return array;
    }

    
    public PdfStream getAsStream(int idx) {
        PdfStream stream = null;
        PdfObject orig = getDirectObject(idx);
        if (orig != null && orig.isStream())
            stream = (PdfStream) orig;
        return stream;
    }

    
    public PdfString getAsString(int idx) {
        PdfString string = null;
        PdfObject orig = getDirectObject(idx);
        if (orig != null && orig.isString())
            string = (PdfString) orig;
        return string;
    }

    
    public PdfNumber getAsNumber(int idx) {
        PdfNumber number = null;
        PdfObject orig = getDirectObject(idx);
        if (orig != null && orig.isNumber())
            number = (PdfNumber) orig;
        return number;
    }
    
    
    public PdfName getAsName(int idx) {
        PdfName name = null;
        PdfObject orig = getDirectObject(idx);
        if (orig != null && orig.isName())
            name = (PdfName) orig;
        return name;
    }

    
    public PdfBoolean getAsBoolean(int idx) {
        PdfBoolean bool = null;
        PdfObject orig = getDirectObject(idx);
        if (orig != null && orig.isBoolean())
            bool = (PdfBoolean) orig;
        return bool;
    }

    
    public PdfIndirectReference getAsIndirectObject(int idx) {
        PdfIndirectReference ref = null;
        PdfObject orig = getPdfObject(idx); 
        if (orig != null && orig.isIndirect())
            ref = (PdfIndirectReference) orig;
        return ref;
    }
}
