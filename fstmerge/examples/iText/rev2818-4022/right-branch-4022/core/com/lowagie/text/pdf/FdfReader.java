
package com.lowagie.text.pdf;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;

public class FdfReader extends PdfReader {
    
    HashMap<String, PdfDictionary> fields;
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
        fields = new HashMap<String, PdfDictionary>();
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
        PdfArray kids = merged.getAsArray(PdfName.KIDS);
        if (kids == null || kids.isEmpty()) {
            if (name.length() > 0)
                name = name.substring(1);
            fields.put(name, merged);
        }
        else {
            merged.remove(PdfName.KIDS);
            for (int k = 0; k < kids.size(); ++k) {
                PdfDictionary dic = new PdfDictionary();
                dic.merge(merged);
                PdfDictionary newDic = kids.getAsDict(k);
                PdfString t = newDic.getAsString(PdfName.T);
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
        catalog = trailer.getAsDict(PdfName.ROOT);
        PdfDictionary fdf = catalog.getAsDict(PdfName.FDF);
        if (fdf == null)
            return;
        PdfString fs = fdf.getAsString(PdfName.F);
        if (fs != null)
            fileSpec = fs.toUnicodeString();
        PdfArray fld = fdf.getAsArray(PdfName.FIELDS);
        if (fld == null)
            return;
        encoding = fdf.getAsName(PdfName.ENCODING);
        PdfDictionary merged = new PdfDictionary();
        merged.put(PdfName.KIDS, fld);
        kidNode(merged, "");
    }

        
    public HashMap<String, PdfDictionary> getFields() {
        return fields;
    }
    
        
    public PdfDictionary getField(String name) {
        return fields.get(name);
    }
    
        
    public String getFieldValue(String name) {
        PdfDictionary field = fields.get(name);
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