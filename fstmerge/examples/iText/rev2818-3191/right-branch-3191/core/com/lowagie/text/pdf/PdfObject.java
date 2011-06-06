

package com.lowagie.text.pdf;
import java.io.IOException;
import java.io.OutputStream;



public abstract class PdfObject {
    
    
    

    public static final int BOOLEAN = 1;
    

    public static final int NUMBER = 2;
    

    public static final int STRING = 3;
    

    public static final int NAME = 4;
    

    public static final int ARRAY = 5;
    

    public static final int DICTIONARY = 6;
    

    public static final int STREAM = 7;


    public static final int NULL = 8;
    
    
    public static final int INDIRECT = 10;    


    public static final String NOTHING = "";
    

    public static final String TEXT_PDFDOCENCODING = "PDF";
    

    public static final String TEXT_UNICODE = "UnicodeBig";
    
    
    

    protected byte[] bytes;
    

    protected int type;
    
    
    protected PRIndirectReference indRef;
    
    
    

    
    protected PdfObject(int type) {
        this.type = type;
    }
    

    
    protected PdfObject(int type, String content) {
        this.type = type;
        bytes = PdfEncodings.convertToBytes(content, null);
    }
    

    
    protected PdfObject(int type, byte[] bytes) {
        this.bytes = bytes;
        this.type = type;
    }
    
    
    

    
    public void toPdf(PdfWriter writer, OutputStream os) throws IOException {
        if (bytes != null)
            os.write(bytes);
    }
    
    
    public byte[] getBytes() {
        return bytes;
    }

    
    public boolean canBeInObjStm() {
        return (type >= 1 && type <= 6) || type == 8;
    }
    

    



    

    
    public String toString() {
        if (bytes == null)
            return super.toString();
        else
            return PdfEncodings.convertToString(bytes, null);
    }
    

    
    public int length() {
        return toString().length();
    }
    

    
    protected void setContent(String content) {
        bytes = PdfEncodings.convertToBytes(content, null);
    }
    
    
    

    
    public int type() {
        return type;
    }
    

    
    public boolean isNull() {
        return (this.type == NULL);
    }
    

    
    public boolean isBoolean() {
        return (this.type == BOOLEAN);
    }
    

    
    public boolean isNumber() {
        return (this.type == NUMBER);
    }
    

    
    public boolean isString() {
        return (this.type == STRING);
    }
    

    
    public boolean isName() {
        return (this.type == NAME);
    }
    

    
    public boolean isArray() {
        return (this.type == ARRAY);
    }
    

    
    public boolean isDictionary() {
        return (this.type == DICTIONARY);
    }
    

    
    public boolean isStream() {
        return (this.type == STREAM);
    }

    
    public boolean isIndirect() {
        return (this.type == INDIRECT);
    }
    
    
    public PRIndirectReference getIndRef() {
        return this.indRef;
    }
    
    
    public void setIndRef(PRIndirectReference indRef) {
        this.indRef = indRef;
    }
}
