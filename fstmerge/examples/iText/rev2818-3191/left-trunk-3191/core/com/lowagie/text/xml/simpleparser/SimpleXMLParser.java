
package com.lowagie.text.xml.simpleparser;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.HashMap;
import java.util.Stack;


public class SimpleXMLParser {
    
    private final static int UNKNOWN = 0;
    private final static int TEXT = 1;
    private final static int TAG_ENCOUNTERED = 2;
    private final static int EXAMIN_TAG = 3;
    private final static int TAG_EXAMINED = 4;
    private final static int IN_CLOSETAG = 5;
    private final static int SINGLE_TAG = 6;
    private final static int CDATA = 7;
    private final static int COMMENT = 8;
    private final static int PI = 9;
    private final static int ENTITY = 10;
    private final static int QUOTE = 11;
    private final static int ATTRIBUTE_KEY = 12;
    private final static int ATTRIBUTE_EQUAL = 13;
    private final static int ATTRIBUTE_VALUE = 14;
    
    
    protected Stack stack;
    
    protected int character = 0;
    
    protected int previousCharacter = -1;
    
    protected int lines = 1;
    
    protected int columns = 0;
    
    protected boolean eol = false;
    
    protected int state;
    
    protected boolean html;
    
    protected StringBuffer text = new StringBuffer();
    
    protected StringBuffer entity = new StringBuffer();
    
    protected String tag = null;
    
    protected HashMap attributes = null;
    
    protected SimpleXMLDocHandler doc;
    
    protected SimpleXMLDocHandlerComment comment;
    
    int nested = 0;
    
    protected int quoteCharacter = '"';
    
    String attributekey = null;
    
    String attributevalue = null;
    
    
    private SimpleXMLParser(SimpleXMLDocHandler doc, SimpleXMLDocHandlerComment comment, boolean html) {
        this.doc = doc;
        this.comment = comment;
        this.html = html;
        stack = new Stack();
        state = html ? TEXT : UNKNOWN;
    }
    
    
    private void go(Reader r) throws IOException {
        BufferedReader reader;
        if (r instanceof BufferedReader)
            reader = (BufferedReader)r;
        else
            reader = new BufferedReader(r);
        doc.startDocument();
        while(true) {
            
            if (previousCharacter == -1) {
                character = reader.read();
            }
            
            else {
                character = previousCharacter;
                previousCharacter = -1;
            }
            
            
            if (character == -1) {
                if (html) {
                    if (html && state == TEXT)
                        flush();
                    doc.endDocument();
                } else {
                    throwException("Missing end tag");
                }
                return;
            }
            
            
            if (character == '\n' && eol) {
                eol = false;
                continue;
            } else if (eol) {
                eol = false;
            } else if (character == '\n') {
                lines++;
                columns = 0;
            } else if (character == '\r') {
                eol = true;
                character = '\n';
                lines++;
                columns = 0;
            } else {
                columns++;
            }
            
            switch(state) {
            
            case UNKNOWN:
                if(character == '<') {
                    saveState(TEXT);
                    state = TAG_ENCOUNTERED;
                }
                break;
            
            case TEXT:
                if(character == '<') {
                    flush();
                    saveState(state);
                    state = TAG_ENCOUNTERED;
                } else if(character == '&') {
                    saveState(state);
                    entity.setLength(0);
                    state = ENTITY;
                } else
                    text.append((char)character);
                break;
            
            
            case TAG_ENCOUNTERED:
                initTag();
                if(character == '/') {
                    state = IN_CLOSETAG;
                } else if (character == '?') {
                    restoreState();
                    state = PI;
                } else {
                    text.append((char)character);
                    state = EXAMIN_TAG;
                }
                break;
            
            
            case EXAMIN_TAG:
                if(character == '>') {
                    doTag();
                    processTag(true);
                    initTag();
                    state = restoreState();
                } else if(character == '/') {
                    state = SINGLE_TAG;
                } else if(character == '-' && text.toString().equals("!-")) {
                    flush();
                    state = COMMENT;
                } else if(character == '[' && text.toString().equals("![CDATA")) {
                    flush();
                    state = CDATA;
                } else if(character == 'E' && text.toString().equals("!DOCTYP")) {
                    flush();
                    state = PI;
                } else if(Character.isWhitespace((char)character)) {
                    doTag();
                    state = TAG_EXAMINED;
                } else {
                    text.append((char)character);
                }
                break;
            
            case TAG_EXAMINED:
                if(character == '>') {
                    processTag(true);
                    initTag();
                    state = restoreState();
                } else if(character == '/') {
                    state = SINGLE_TAG;
                } else if(Character.isWhitespace((char)character)) {
                    
                } else {
                    text.append((char)character);
                    state = ATTRIBUTE_KEY;
                }
                break;
                
                
            case IN_CLOSETAG:
                if(character == '>') {
                    doTag();
                    processTag(false);
                    if(!html && nested==0) return;
                    state = restoreState();
                } else {
                    if (!Character.isWhitespace((char)character))
                        text.append((char)character);
                }
                break;
                
            
            
            case SINGLE_TAG:
                if(character != '>')
                    throwException("Expected > for tag: <"+tag+"/>");
                doTag();
                processTag(true);
                processTag(false);
                initTag();
                if(!html && nested==0) {
                    doc.endDocument();
                    return;
                }
                state = restoreState();
                break;
                
            
            case CDATA:
                if(character == '>'
                && text.toString().endsWith("]]")) {
                    text.setLength(text.length()-2);
                    flush();
                    state = restoreState();
                } else
                    text.append((char)character);
                break;
                
            
            
            case COMMENT:
                if(character == '>'
                && text.toString().endsWith("--")) {
                    text.setLength(text.length() - 2);
                    flush();
                    state = restoreState();
                } else
                    text.append((char)character);
                break;
                
            
            case PI:
                if(character == '>') {
                    state = restoreState();
                    if(state == TEXT) state = UNKNOWN;
                }
                break;
                
            
            case ENTITY:
                if(character == ';') {
                    state = restoreState();
                    String cent = entity.toString();
                    entity.setLength(0);
                    char ce = EntitiesToUnicode.decodeEntity(cent);
                    if (ce == '\0')
                        text.append('&').append(cent).append(';');
                    else
                        text.append(ce);
                } else if ((character != '#' && (character < '0' || character > '9') && (character < 'a' || character > 'z')
                    && (character < 'A' || character > 'Z')) || entity.length() >= 7) {
                    state = restoreState();
                    previousCharacter = character;
                    text.append('&').append(entity.toString());
                    entity.setLength(0);
                }
                else {
                    entity.append((char)character);
                }
                break;
            
            case QUOTE:
                if (html && quoteCharacter == ' ' && character == '>') {
                    flush();
                    processTag(true);
                    initTag();
                    state = restoreState();
                }
                else if (html && quoteCharacter == ' ' && Character.isWhitespace((char)character)) {
                    flush();
                    state = TAG_EXAMINED;
                }
                else if (html && quoteCharacter == ' ') {
                    text.append((char)character);
                }
                else if(character == quoteCharacter) {
                    flush();
                    state = TAG_EXAMINED;
                } else if(" \r\n\u".indexOf(character)>=0) {
                    text.append(' ');
                } else if(character == '&') {
                    saveState(state);
                    state = ENTITY;
                    entity.setLength(0);
                } else {
                    text.append((char)character);
                }
                break;
                
            case ATTRIBUTE_KEY:
                if(Character.isWhitespace((char)character)) {
                    flush();
                    state = ATTRIBUTE_EQUAL;
                } else if(character == '=') {
                    flush();
                    state = ATTRIBUTE_VALUE;
                } else if (html && character == '>') {
                    text.setLength(0);
                    processTag(true);
                    initTag();
                    state = restoreState();
                } else {
                    text.append((char)character);
                }
                break;
                
            case ATTRIBUTE_EQUAL:
                if(character == '=') {
                    state = ATTRIBUTE_VALUE;
                } else if(Character.isWhitespace((char)character)) {
                    
                } else if (html && character == '>') {
                    text.setLength(0);
                    processTag(true);
                    initTag();
                    state = restoreState();
                } else if (html && character == '/') {
                    flush();
                    state = SINGLE_TAG;
                } else if (html) {
                    flush();
                    text.append((char)character);
                    state = ATTRIBUTE_KEY;
                } else {
                    throwException("Error in attribute processing.");
                }
                break;
                
            case ATTRIBUTE_VALUE:
                if(character == '"' || character == '\'') {
                    quoteCharacter = character;
                    state = QUOTE;
                } else if(Character.isWhitespace((char)character)) {
                    
                } else if (html && character == '>') {
                    flush();
                    processTag(true);
                    initTag();
                    state = restoreState();
                } else if (html) {
                    text.append((char)character);
                    quoteCharacter = ' ';
                    state = QUOTE;
                } else {
                    throwException("Error in attribute processing");
                }
                break;
            }
        }
    }

    
    private int restoreState() {
        if(!stack.empty())
            return ((Integer)stack.pop()).intValue();
        else
            return UNKNOWN;
    }
    
    private void saveState(int s) {
        stack.push(new Integer(s));
    }
    
    private void flush() {
        switch(state){
        case TEXT:
        case CDATA:
            if(text.length() > 0) {
                doc.text(text.toString());
            }
            break;
        case COMMENT:
            if (comment != null) {
                comment.comment(text.toString());
            }
            break;
        case ATTRIBUTE_KEY:
            attributekey = text.toString();
            if (html)
                attributekey = attributekey.toLowerCase();
            break;
        case QUOTE:
        case ATTRIBUTE_VALUE:
            attributevalue = text.toString();
            attributes.put(attributekey,attributevalue);
            break;
        default:
            
        }
        text.setLength(0);
    }
    
    private void initTag() {
        tag = null;
        attributes = new HashMap();
    }
    
    private void doTag() {
        if(tag == null)
            tag = text.toString();
        if (html)
            tag = tag.toLowerCase();
        text.setLength(0);
    }
    
    private void processTag(boolean start) {
        if (start) {
            nested++;
            doc.startElement(tag,attributes);
        }
        else {
            nested--;
            doc.endElement(tag);
        }
    }
    
    private void throwException(String s) throws IOException {
        throw new IOException(s+" near line " + lines + ", column " + columns);
    }
    
    
    public static void parse(SimpleXMLDocHandler doc, SimpleXMLDocHandlerComment comment, Reader r, boolean html) throws IOException {
        SimpleXMLParser parser = new SimpleXMLParser(doc, comment, html);
        parser.go(r);
    }
    
        
    public static void parse(SimpleXMLDocHandler doc, InputStream in) throws IOException {
        byte b4[] = new byte[4];
        int count = in.read(b4);
        if (count != 4)
            throw new IOException("Insufficient length.");
        String encoding = getEncodingName(b4);
        String decl = null;
        if (encoding.equals("UTF-8")) {
            StringBuffer sb = new StringBuffer();
            int c;
            while ((c = in.read()) != -1) {
                if (c == '>')
                    break;
                sb.append((char)c);
            }
            decl = sb.toString();
        }
        else if (encoding.equals("CP037")) {
            ByteArrayOutputStream bi = new ByteArrayOutputStream();
            int c;
            while ((c = in.read()) != -1) {
                if (c == 0x6e) 
                    break;
                bi.write(c);
            }
            decl = new String(bi.toByteArray(), "CP037");
        }
        if (decl != null) {
            decl = getDeclaredEncoding(decl);
            if (decl != null)
                encoding = decl;
        }
        parse(doc, new InputStreamReader(in, IanaEncodings.getJavaEncoding(encoding)));
    }
    
    private static String getDeclaredEncoding(String decl) {
        if (decl == null)
            return null;
        int idx = decl.indexOf("encoding");
        if (idx < 0)
            return null;
        int idx1 = decl.indexOf('"', idx);
        int idx2 = decl.indexOf('\'', idx);
        if (idx1 == idx2)
            return null;
        if ((idx1 < 0 && idx2 > 0) || (idx2 > 0 && idx2 < idx1)) {
            int idx3 = decl.indexOf('\'', idx2 + 1);
            if (idx3 < 0)
                return null;
            return decl.substring(idx2 + 1, idx3);
        }
        if ((idx2 < 0 && idx1 > 0) || (idx1 > 0 && idx1 < idx2)) {
            int idx3 = decl.indexOf('"', idx1 + 1);
            if (idx3 < 0)
                return null;
            return decl.substring(idx1 + 1, idx3);
        }
        return null;
    }
    
    public static void parse(SimpleXMLDocHandler doc,Reader r) throws IOException {
        parse(doc, null, r, false);
    }
    
        
    public static String escapeXML(String s, boolean onlyASCII) {
        char cc[] = s.toCharArray();
        int len = cc.length;
        StringBuffer sb = new StringBuffer();
        for (int k = 0; k < len; ++k) {
            int c = cc[k];
            switch (c) {
                case '<':
                    sb.append("&lt;");
                    break;
                case '>':
                    sb.append("&gt;");
                    break;
                case '&':
                    sb.append("&amp;");
                    break;
                case '"':
                    sb.append("&quot;");
                    break;
                case '\'':
                    sb.append("&apos;");
                    break;
                default:
                    if (onlyASCII && c > 127)
                        sb.append("&#").append(c).append(';');
                    else
                        sb.append((char)c);
            }
        }
        return sb.toString();
    }
    
    private static String getEncodingName(byte[] b4) {
        
        
        int b0 = b4[0] & 0xFF;
        int b1 = b4[1] & 0xFF;
        if (b0 == 0xFE && b1 == 0xFF) {
            
            return "UTF-16BE";
        }
        if (b0 == 0xFF && b1 == 0xFE) {
            
            return "UTF-16LE";
        }
        
        
        int b2 = b4[2] & 0xFF;
        if (b0 == 0xEF && b1 == 0xBB && b2 == 0xBF) {
            return "UTF-8";
        }
        
        
        int b3 = b4[3] & 0xFF;
        if (b0 == 0x00 && b1 == 0x00 && b2 == 0x00 && b3 == 0x3C) {
            
            return "ISO-10646-UCS-4";
        }
        if (b0 == 0x3C && b1 == 0x00 && b2 == 0x00 && b3 == 0x00) {
            
            return "ISO-10646-UCS-4";
        }
        if (b0 == 0x00 && b1 == 0x00 && b2 == 0x3C && b3 == 0x00) {
            
            
            return "ISO-10646-UCS-4";
        }
        if (b0 == 0x00 && b1 == 0x3C && b2 == 0x00 && b3 == 0x00) {
            
            
            return "ISO-10646-UCS-4";
        }
        if (b0 == 0x00 && b1 == 0x3C && b2 == 0x00 && b3 == 0x3F) {
            
            
            
            return "UTF-16BE";
        }
        if (b0 == 0x3C && b1 == 0x00 && b2 == 0x3F && b3 == 0x00) {
            
            
            return "UTF-16LE";
        }
        if (b0 == 0x4C && b1 == 0x6F && b2 == 0xA7 && b3 == 0x94) {
            
            
            return "CP037";
        }
        
        
        return "UTF-8";
    }
}