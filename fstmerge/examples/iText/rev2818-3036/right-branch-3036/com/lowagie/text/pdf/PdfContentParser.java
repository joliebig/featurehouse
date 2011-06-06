

package com.lowagie.text.pdf;

import java.io.IOException;
import java.util.ArrayList;

public class PdfContentParser {
    
        
    public static final int COMMAND_TYPE = 200;
    
    private PRTokeniser tokeniser;    
    
    
    public PdfContentParser(PRTokeniser tokeniser) {
        this.tokeniser = tokeniser;
    }
    
        
    public ArrayList<PdfObject> parse(ArrayList<PdfObject> ls) throws IOException {
        if (ls == null)
            ls = new ArrayList<PdfObject>();
        else
            ls.clear();
        PdfObject ob = null;
        while ((ob = readPRObject()) != null) {
            ls.add(ob);
            if (ob.type() == COMMAND_TYPE)
                break;
        }
        return ls;
    }
    
    
    public PRTokeniser getTokeniser() {
        return this.tokeniser;
    }
    
    
    public void setTokeniser(PRTokeniser tokeniser) {
        this.tokeniser = tokeniser;
    }
    
        
    public PdfDictionary readDictionary() throws IOException {
        PdfDictionary dic = new PdfDictionary();
        while (true) {
            if (!nextValidToken())
                throw new IOException("Unexpected end of file.");
                if (tokeniser.getTokenType() == PRTokeniser.TK_END_DIC)
                    break;
                if (tokeniser.getTokenType() != PRTokeniser.TK_NAME)
                    throw new IOException("Dictionary key is not a name.");
                PdfName name = new PdfName(tokeniser.getStringValue(), false);
                PdfObject obj = readPRObject();
                int type = obj.type();
                if (-type == PRTokeniser.TK_END_DIC)
                    throw new IOException("Unexpected '>>'");
                if (-type == PRTokeniser.TK_END_ARRAY)
                    throw new IOException("Unexpected ']'");
                dic.put(name, obj);
        }
        return dic;
    }
    
        
    public PdfArray readArray() throws IOException {
        PdfArray array = new PdfArray();
        while (true) {
            PdfObject obj = readPRObject();
            int type = obj.type();
            if (-type == PRTokeniser.TK_END_ARRAY)
                break;
            if (-type == PRTokeniser.TK_END_DIC)
                throw new IOException("Unexpected '>>'");
            array.add(obj);
        }
        return array;
    }
    
        
    public PdfObject readPRObject() throws IOException {
        if (!nextValidToken())
            return null;
        int type = tokeniser.getTokenType();
        switch (type) {
            case PRTokeniser.TK_START_DIC: {
                PdfDictionary dic = readDictionary();
                return dic;
            }
            case PRTokeniser.TK_START_ARRAY:
                return readArray();
            case PRTokeniser.TK_STRING:
                PdfString str = new PdfString(tokeniser.getStringValue(), null).setHexWriting(tokeniser.isHexString());
                return str;
            case PRTokeniser.TK_NAME:
                return new PdfName(tokeniser.getStringValue(), false);
            case PRTokeniser.TK_NUMBER:
                return new PdfNumber(tokeniser.getStringValue());
            case PRTokeniser.TK_OTHER:
                return new PdfLiteral(COMMAND_TYPE, tokeniser.getStringValue());
            default:
                return new PdfLiteral(-type, tokeniser.getStringValue());
        }
    }
    
        
    public boolean nextValidToken() throws IOException {
        while (tokeniser.nextToken()) {
            if (tokeniser.getTokenType() == PRTokeniser.TK_COMMENT)
                continue;
            return true;
        }
        return false;
    }
}