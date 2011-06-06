
package com.lowagie.text.pdf;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import com.lowagie.text.DocWriter;


public class FdfWriter {
    private static final byte[] HEADER_FDF = DocWriter.getISOBytes("%FDF-1.2\n%\u\u\u\u\n");
    HashMap<String, Object> fields = new HashMap<String, Object>();

    
    private String file;
    
        
    public FdfWriter() {
    }

        
    public void writeTo(OutputStream os) throws IOException {
        Wrt wrt = new Wrt(os, this);
        wrt.writeTo();
    }

    @SuppressWarnings("unchecked")
    boolean setField(String field, PdfObject value) {
        HashMap<String, Object> map = fields;
        StringTokenizer tk = new StringTokenizer(field, ".");
        if (!tk.hasMoreTokens())
            return false;
        while (true) {
            String s = tk.nextToken();
            Object obj = map.get(s);
            if (tk.hasMoreTokens()) {
                if (obj == null) {
                    obj = new HashMap<String, Object>();
                    map.put(s, obj);
                    map = (HashMap<String, Object>)obj;
                    continue;
                }
                else if (obj instanceof HashMap)
                    map = (HashMap<String, Object>)obj;
                else
                    return false;
            }
            else {
                if (!(obj instanceof HashMap)) {
                    map.put(s, value);
                    return true;
                }
                else
                    return false;
            }
        }
    }

    @SuppressWarnings("unchecked")
    void iterateFields(HashMap<String, Object> values, HashMap<String, Object> map, String name) {
        for (Map.Entry<String, Object> entry: map.entrySet()) {
            String s = entry.getKey();
            Object obj = entry.getValue();
            if (obj instanceof HashMap)
                iterateFields(values, (HashMap<String, Object>)obj, name + "." + s);
            else
                values.put((name + "." + s).substring(1), obj);
        }
    }
    
    
    @SuppressWarnings("unchecked")
    public boolean removeField(String field) {
        HashMap<String, Object> map = fields;
        StringTokenizer tk = new StringTokenizer(field, ".");
        if (!tk.hasMoreTokens())
            return false;
        ArrayList<Object> hist = new ArrayList<Object>();
        while (true) {
            String s = tk.nextToken();
            Object obj = map.get(s);
            if (obj == null)
                return false;
            hist.add(map);
            hist.add(s);
            if (tk.hasMoreTokens()) {
                if (obj instanceof HashMap)
                    map = (HashMap<String, Object>)obj;
                else
                    return false;
            }
            else {
                if (obj instanceof HashMap)
                    return false;
                else
                    break;
            }
        }
        for (int k = hist.size() - 2; k >= 0; k -= 2) {
            map = (HashMap<String, Object>)hist.get(k);
            String s = (String)hist.get(k + 1);
            map.remove(s);
            if (!map.isEmpty())
                break;
        }
        return true;
    }
    
        
    public HashMap<String, Object> getFields() {
        HashMap<String, Object> values = new HashMap<String, Object>();
        iterateFields(values, fields, "");
        return values;
    }
    
    
    @SuppressWarnings("unchecked")
    public String getField(String field) {
        HashMap<String, Object> map = fields;
        StringTokenizer tk = new StringTokenizer(field, ".");
        if (!tk.hasMoreTokens())
            return null;
        while (true) {
            String s = tk.nextToken();
            Object obj = map.get(s);
            if (obj == null)
                return null;
            if (tk.hasMoreTokens()) {
                if (obj instanceof HashMap)
                    map = (HashMap<String, Object>)obj;
                else
                    return null;
            }
            else {
                if (obj instanceof HashMap)
                    return null;
                else {
                    if (((PdfObject)obj).isString())
                        return ((PdfString)obj).toUnicodeString();
                    else
                        return PdfName.decodeName(obj.toString());
                }
            }
        }
    }
    
        
    public boolean setFieldAsName(String field, String value) {
        return setField(field, new PdfName(value));
    }
    
        
    public boolean setFieldAsString(String field, String value) {
        return setField(field, new PdfString(value, PdfObject.TEXT_UNICODE));
    }
    
        
    public void setFields(FdfReader fdf) {
        HashMap<String, PdfDictionary> map = fdf.getFields();
        for (Map.Entry<String, PdfDictionary> entry: map.entrySet()) {
            String key = entry.getKey();
            PdfDictionary dic = entry.getValue();
            PdfObject v = dic.get(PdfName.V);
            if (v != null) {
                setField(key, v);
            }
        }
    }
    
        
    public void setFields(PdfReader pdf) {
        setFields(pdf.getAcroFields());
    }
    
        
    public void setFields(AcroFields af) {
        for (Map.Entry<String, AcroFields.Item> entry: af.getFields().entrySet()) {
            String fn = entry.getKey();
            AcroFields.Item item = entry.getValue();
            PdfDictionary dic = item.merged.get(0);
            PdfObject v = PdfReader.getPdfObjectRelease(dic.get(PdfName.V));
            if (v == null)
                continue;
            PdfObject ft = PdfReader.getPdfObjectRelease(dic.get(PdfName.FT));
            if (ft == null || PdfName.SIG.equals(ft))
                continue;
            setField(fn, v);
        }
    }
    
    
    public String getFile() {
        return this.file;
    }
    
    
    public void setFile(String file) {
        this.file = file;
    }
    
    static class Wrt extends PdfWriter {
        private FdfWriter fdf;
       
        Wrt(OutputStream os, FdfWriter fdf) throws IOException {
            super(new PdfDocument(), os);
            this.fdf = fdf;
            this.os.write(HEADER_FDF);
            body = new PdfBody(this);
        }
        
        void writeTo() throws IOException {
            PdfDictionary dic = new PdfDictionary();
            dic.put(PdfName.FIELDS, calculate(fdf.fields));
            if (fdf.file != null)
                dic.put(PdfName.F, new PdfString(fdf.file, PdfObject.TEXT_UNICODE));
            PdfDictionary fd = new PdfDictionary();
            fd.put(PdfName.FDF, dic);
            PdfIndirectReference ref = addToBody(fd).getIndirectReference();
            os.write(getISOBytes("trailer\n"));
            PdfDictionary trailer = new PdfDictionary();
            trailer.put(PdfName.ROOT, ref);
            trailer.toPdf(null, os);
            os.write(getISOBytes("\n%%EOF\n"));
            os.close();
        }

        @SuppressWarnings("unchecked")
        PdfArray calculate(HashMap<String, Object> map) throws IOException {
            PdfArray ar = new PdfArray();
            for (Map.Entry<String, Object> entry: map.entrySet()) {
                String key = entry.getKey();
                Object v = entry.getValue();
                PdfDictionary dic = new PdfDictionary();
                dic.put(PdfName.T, new PdfString(key, PdfObject.TEXT_UNICODE));
                if (v instanceof HashMap) {
                    dic.put(PdfName.KIDS, calculate((HashMap<String, Object>)v));
                }
                else {
                    dic.put(PdfName.V, (PdfObject)v);
                }
                ar.add(dic);
            }
            return ar;
        }
    }
}
