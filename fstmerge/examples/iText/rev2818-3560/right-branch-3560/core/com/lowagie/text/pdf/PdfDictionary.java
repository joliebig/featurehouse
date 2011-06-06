

package com.lowagie.text.pdf;

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;



public class PdfDictionary extends PdfObject {
    
    
    

    public static final PdfName FONT = PdfName.FONT;
    

    public static final PdfName OUTLINES = PdfName.OUTLINES;
    

    public static final PdfName PAGE = PdfName.PAGE;
    

    public static final PdfName PAGES = PdfName.PAGES;
    

    public static final PdfName CATALOG = PdfName.CATALOG;
    
    
    

    private PdfName dictionaryType = null;
    

    protected HashMap<PdfName, PdfObject> hashMap;
    
    
    

    
    public PdfDictionary() {
        super(DICTIONARY);
        hashMap = new HashMap<PdfName, PdfObject>();
    }
    

    
    public PdfDictionary(PdfName type) {
        this();
        dictionaryType = type;
        put(PdfName.TYPE, dictionaryType);
    }
    
    
    

    
    public void toPdf(PdfWriter writer, OutputStream os) throws IOException {
        os.write('<');
        os.write('<');

        
        PdfName key;
        PdfObject value;
        int type = 0;
        for (Iterator<PdfName> i = hashMap.keySet().iterator(); i.hasNext(); ) {
            key = i.next();
            value = hashMap.get(key);
            key.toPdf(writer, os);
            type = value.type();
            if (type != PdfObject.ARRAY && type != PdfObject.DICTIONARY && type != PdfObject.NAME && type != PdfObject.STRING)
                os.write(' ');
            value.toPdf(writer, os);
        }
        os.write('>');
        os.write('>');
    }
    
    
    

    
    public void put(PdfName key, PdfObject value) {
        if (value == null || value.isNull())
            hashMap.remove(key);
        else
            hashMap.put(key, value);
    }
    

    public void putEx(PdfName key, PdfObject value) {
        if (value == null)
            return;
        put(key, value);
    }
    

    
    public void remove(PdfName key) {
        hashMap.remove(key);
    }
    

    
    public PdfObject get(PdfName key) {
        return hashMap.get(key);
    }
    
    
    

    
    public boolean isFont() {
        return FONT.equals(dictionaryType);
    }
    

    
    public boolean isPage() {
        return PAGE.equals(dictionaryType);
    }
    

    
    public boolean isPages() {
        return PAGES.equals(dictionaryType);
    }
    

    
    public boolean isCatalog() {
        return CATALOG.equals(dictionaryType);
    }
    

    
    public boolean isOutlineTree() {
        return OUTLINES.equals(dictionaryType);
    }
    
    public void merge(PdfDictionary other) {
        hashMap.putAll(other.hashMap);
    }
    
    public void mergeDifferent(PdfDictionary other) {
        for (Iterator<PdfName> i = other.hashMap.keySet().iterator(); i.hasNext();) {
            PdfName key = i.next();
            if (!hashMap.containsKey(key)) {
                hashMap.put(key, other.hashMap.get(key));
            }
        }
    }
    
    public Set<PdfName> getKeys() {
        return hashMap.keySet();
    }

    public void putAll(PdfDictionary dic) {
        hashMap.putAll(dic.hashMap);
    }
    
    public int size() {
        return hashMap.size();
    }
    
    public boolean contains(PdfName key) {
        return hashMap.containsKey(key);
    }
    
    
    public String toString() {
        if (get(PdfName.TYPE) == null) return "Dictionary";
        return "Dictionary of type: " + get(PdfName.TYPE);
    }
    
    
    public PdfObject getDirectObject(PdfName key) {
        return PdfReader.getPdfObject(get(key));
    }
    
    
    public PdfDictionary getAsDict(PdfName key) {
        PdfDictionary dict = null;
        PdfObject orig = getDirectObject(key);
        if (orig != null && orig.isDictionary())
            dict = (PdfDictionary) orig;
        return dict;
    }
    
    public PdfArray getAsArray(PdfName key) {
        PdfArray array = null;
        PdfObject orig = getDirectObject(key);
        if (orig != null && orig.isArray())
            array = (PdfArray) orig;
        return array;
    }
    
    public PdfStream getAsStream(PdfName key) {
        PdfStream stream = null;
        PdfObject orig = getDirectObject(key);
        if (orig != null && orig.isStream())
            stream = (PdfStream) orig;
        return stream;
    }
    
    public PdfString getAsString(PdfName key) {
        PdfString string = null;
        PdfObject orig = getDirectObject(key);
        if (orig != null && orig.isString())
            string = (PdfString) orig;
        return string;
    }
    
    public PdfNumber getAsNumber(PdfName key) {
        PdfNumber number = null;
        PdfObject orig = getDirectObject(key);
        if (orig != null && orig.isNumber())
            number = (PdfNumber) orig;
        return number;
    }
    
    public PdfName getAsName(PdfName key) {
        PdfName name = null;
        PdfObject orig = getDirectObject(key);
        if (orig != null && orig.isName())
            name = (PdfName) orig;
        return name;
    }
    
    public PdfBoolean getAsBoolean(PdfName key) {
        PdfBoolean bool = null;
        PdfObject orig = getDirectObject(key);
        if (orig != null && orig.isBoolean())
            bool = (PdfBoolean)orig;
        return bool;
    }
    
    public PdfIndirectReference getAsIndirectObject( PdfName key ) {
        PdfIndirectReference ref = null;
        PdfObject orig = get(key); 
        if (orig != null && orig.isIndirect())
            ref = (PdfIndirectReference) orig;
        return ref;
    }
}