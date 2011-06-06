

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

    
    public String toString() {
        if (bytes == null)
            return super.toString();
        return PdfEncodings.convertToString(bytes, null);
    }

    
    public byte[] getBytes() {
        return bytes;
    }

    
    public boolean canBeInObjStm() {
        switch (type) {
            case NULL:
            case BOOLEAN:
            case NUMBER:
            case STRING:
            case NAME:
            case ARRAY:
            case DICTIONARY:
                return true;
            case STREAM:
            case INDIRECT:
            default:
                return false;
        }
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
        return (type == NULL);
    }

    
    public boolean isBoolean() {
        return (type == BOOLEAN);
    }

    
    public boolean isNumber() {
        return (type == NUMBER);
    }

    
    public boolean isString() {
        return (type == STRING);
    }

    
    public boolean isName() {
        return (type == NAME);
    }

    
    public boolean isArray() {
        return (type == ARRAY);
    }

    
    public boolean isDictionary() {
        return (type == DICTIONARY);
    }

    
    public boolean isStream() {
        return (type == STREAM);
    }

    
    public boolean isIndirect() {
        return (type == INDIRECT);
    }

    
    public PRIndirectReference getIndRef() {
        return indRef;
    }

    
    public void setIndRef(PRIndirectReference indRef) {
        this.indRef = indRef;
    }
}
