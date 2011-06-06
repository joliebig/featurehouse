

package com.lowagie.text.pdf;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;



public class PRAcroForm extends PdfDictionary {
    
    
    public static class FieldInformation {
        String name;
        PdfDictionary info;
        PRIndirectReference ref;
        
        FieldInformation(String name, PdfDictionary info, PRIndirectReference ref) {
            this.name = name; this.info = info; this.ref = ref;
        }
        public String getName() { return name; }
        public PdfDictionary getInfo() { return info; }
        public PRIndirectReference getRef() { return ref; }
    };
    ArrayList<FieldInformation> fields;
    ArrayList<PdfDictionary> stack;
    HashMap<String, FieldInformation> fieldByName;
    PdfReader reader;
    
    
    public PRAcroForm(PdfReader reader) {
        this.reader = reader;
        fields = new ArrayList<FieldInformation>();
        fieldByName = new HashMap<String, FieldInformation>();
        stack = new ArrayList<PdfDictionary>();
    }
    
    public int size() {
        return fields.size();
    }
    
    public ArrayList<FieldInformation> getFields() {
        return fields;
    }
    
    public FieldInformation getField(String name) {
        return fieldByName.get(name);
    }
    
    
    public PRIndirectReference getRefByName(String name) {
        FieldInformation fi = fieldByName.get(name);
        if (fi == null) return null;
        return fi.getRef();
    }
    
    public void readAcroForm(PdfDictionary root) {
        if (root == null)
            return;
        hashMap = root.hashMap;
        pushAttrib(root);
        PdfArray fieldlist = (PdfArray)PdfReader.getPdfObjectRelease(root.get(PdfName.FIELDS));
        iterateFields(fieldlist, null, null);
    }
    
    
    protected void iterateFields(PdfArray fieldlist, PRIndirectReference fieldDict, String title) {
        for (Iterator<PdfObject> it = fieldlist.listIterator(); it.hasNext();) {
            PRIndirectReference ref = (PRIndirectReference)it.next();
            PdfDictionary dict = (PdfDictionary) PdfReader.getPdfObjectRelease(ref);
            
            
            PRIndirectReference myFieldDict = fieldDict;
            String myTitle = title;
            PdfString tField = (PdfString)dict.get(PdfName.T);
            boolean isFieldDict = tField != null;
            
            if (isFieldDict) {
                myFieldDict = ref;
                if (title == null) myTitle = tField.toString();
                else myTitle = title + '.' + tField.toString();
            }
            
            PdfArray kids = (PdfArray)dict.get(PdfName.KIDS);
            if (kids != null) {
                pushAttrib(dict);
                iterateFields(kids, myFieldDict, myTitle);
                stack.remove(stack.size() - 1);   
            }
            else {          
                if (myFieldDict != null) {
                    PdfDictionary mergedDict = stack.get(stack.size() - 1);
                    if (isFieldDict)
                        mergedDict = mergeAttrib(mergedDict, dict);
                    
                    mergedDict.put(PdfName.T, new PdfString(myTitle));
                    FieldInformation fi = new FieldInformation(myTitle, mergedDict, myFieldDict);
                    fields.add(fi);
                    fieldByName.put(myTitle, fi);
                }
            }
        }
    }
    
    protected PdfDictionary mergeAttrib(PdfDictionary parent, PdfDictionary child) {
        PdfDictionary targ = new PdfDictionary();
        if (parent != null) targ.putAll(parent);
        
        for (PdfName key: child.getKeys()) {
            if (key.equals(PdfName.DR) || key.equals(PdfName.DA) ||
            key.equals(PdfName.Q)  || key.equals(PdfName.FF) ||
            key.equals(PdfName.DV) || key.equals(PdfName.V)
            || key.equals(PdfName.FT)
            || key.equals(PdfName.F)) {
                targ.put(key,child.get(key));
            }
        }
        return targ;
    }
    
    protected void pushAttrib(PdfDictionary dict) {
        PdfDictionary dic = null;
        if (!stack.isEmpty()) {
            dic = stack.get(stack.size() - 1);
        }
        dic = mergeAttrib(dic, dict);
        stack.add(dic);
    }
}
