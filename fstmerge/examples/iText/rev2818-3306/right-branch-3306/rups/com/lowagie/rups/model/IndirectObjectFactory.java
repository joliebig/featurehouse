

package com.lowagie.rups.model;

import java.util.ArrayList;

import com.lowagie.text.pdf.IntHashtable;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNull;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfReader;


public class IndirectObjectFactory {

    
    protected PdfReader reader;
    
    protected int current;
    
    protected int n;
    
    protected ArrayList<PdfObject> objects = new ArrayList<PdfObject>();
    
    protected IntHashtable idxToRef = new IntHashtable();
    
    protected IntHashtable refToIdx = new IntHashtable();
    
    
    public IndirectObjectFactory(PdfReader reader) {
        this.reader = reader;
        current = -1;
        n = reader.getXrefSize();
    }

    
    public int getCurrent() {
        return current;
    }

    
    public int getXRefMaximum() {
        return n;
    }

    
    public boolean storeNextObject() {
        while (current < n) {
            current++;
            PdfObject object = reader.getPdfObjectRelease(current);
            if (object != null) {
                int idx = size();
                idxToRef.put(idx, current);
                refToIdx.put(current, idx);
                store(object);
                return true;
            }
        }
        return false;
    }
    
    
    private void store(PdfObject object) {
        if (object.isDictionary()){
            PdfDictionary dict = (PdfDictionary)object;
            if (PdfName.PAGE.equals(dict.get(PdfName.TYPE))) {
                objects.add(dict);
                return;
            }
        }
        objects.add(PdfNull.PDFNULL);
    }
    
    
    public int size() {
        return objects.size();
    }
    
    
    public int getIndexByRef(int ref) {
        return refToIdx.get(ref);
    }
    
    
    public int getRefByIndex(int i) {
        return idxToRef.get(i);
    }
    
    
    public PdfObject getObjectByIndex(int i) {
        return getObjectByReference(getRefByIndex(i));
    }

    
    public PdfObject getObjectByReference(int ref) {
        return objects.get(getIndexByRef(ref));
    }
    
    
    public PdfObject loadObjectByReference(int ref) {
        PdfObject object = getObjectByReference(ref);
        if (object instanceof PdfNull) {
            int idx = getIndexByRef(ref);
            object = reader.getPdfObject(ref);
            objects.set(idx, object);
        }
        return object;
    }
}
