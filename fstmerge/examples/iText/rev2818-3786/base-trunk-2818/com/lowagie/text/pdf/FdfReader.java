
package com.lowagie.text.pdf;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;

public class FdfReader extends PdfReader {
    
    HashMap fields;
    String fileSpec;
    PdfName encoding;
    
        
    public FdfReader(String filename) throws IOException {
        super(filename);
    }
    
        
    public FdfReader(byte pdfIn[]) throws IOException {
        super(pdfIn);
    }
    
        
    public FdfReader(URL url) throws IOException {
        super(url);
    }
    
        
    public FdfReader(InputStream is) throws IOException {
        super(is);
    }
    
    protected void readPdf() throws IOException {
        fields = new HashMap();
        try {
            tokens.checkFdfHeader();
            rebuildXref();
            readDocObj();
        }
        finally {
            try {
                tokens.close();
            }
            catch (Exception e) {
                
            }
        }
        readFields();
    }
    
    protected void kidNode(PdfDictionary merged, String name) {
        PdfArray kids = (PdfArray)getPdfObject(merged.get(PdfName.KIDS));
        if (kids == null || kids.getArrayList().size() == 0) {
            if (name.length() > 0)
                name = name.substring(1);
            fields.put(name, merged);
        }
        else {
            merged.remove(PdfName.KIDS);
            ArrayList ar = kids.getArrayList();
            for (int k = 0; k < ar.size(); ++k) {
                PdfDictionary dic = new PdfDictionary();
                dic.merge(merged);
                PdfDictionary newDic = (PdfDictionary)getPdfObject((PdfObject)ar.get(k));
                PdfString t = (PdfString)getPdfObject(newDic.get(PdfName.T));
                String newName = name;
                if (t != null)
                    newName += "." + t.toUnicodeString();
                dic.merge(newDic);
                dic.remove(PdfName.T);
                kidNode(dic, newName);
            }
        }
    }
    
    protected void readFields() {
        catalog = (PdfDictionary)getPdfObject(trailer.get(PdfName.ROOT));
        PdfDictionary fdf = (PdfDictionary)getPdfObject(catalog.get(PdfName.FDF));
        PdfString fs = (PdfString)getPdfObject(fdf.get(PdfName.F));
        if (fs != null)
            fileSpec = fs.toUnicodeString();
        PdfArray fld = (PdfArray)getPdfObject(fdf.get(PdfName.FIELDS));
        if (fld == null)
            return;
        encoding = (PdfName)getPdfObject(fdf.get(PdfName.ENCODING));
        PdfDictionary merged = new PdfDictionary();
        merged.put(PdfName.KIDS, fld);
        kidNode(merged, "");
    }

        
    public HashMap getFields() {
        return fields;
    }
    
        
    public PdfDictionary getField(String name) {
        return (PdfDictionary)fields.get(name);
    }
    
        
    public String getFieldValue(String name) {
        PdfDictionary field = (PdfDictionary)fields.get(name);
        if (field == null)
            return null;
        PdfObject v = getPdfObject(field.get(PdfName.V));
        if (v == null)
            return null;
        if (v.isName())
            return PdfName.decodeName(((PdfName)v).toString());
        else if (v.isString()) {
            PdfString vs = (PdfString)v;
            if (encoding == null || vs.getEncoding() != null)
                return vs.toUnicodeString();
            byte b[] = vs.getBytes();
            if (b.length >= 2 && b[0] == (byte)254 && b[1] == (byte)255)
                return vs.toUnicodeString();
            try {
                if (encoding.equals(PdfName.SHIFT_JIS))
                    return new String(b, "SJIS");
                else if (encoding.equals(PdfName.UHC))
                    return new String(b, "MS949");
                else if (encoding.equals(PdfName.GBK))
                    return new String(b, "GBK");
                else if (encoding.equals(PdfName.BIGFIVE))
                    return new String(b, "Big5");
            }
            catch (Exception e) {
            }
            return vs.toUnicodeString();
        }
        return null;
    }
    
        
    public String getFileSpec() {
        return fileSpec;
    }
}