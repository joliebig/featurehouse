

package com.lowagie.text.pdf.hyphenation;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.StringTokenizer;

import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.html.HtmlTags;
import com.lowagie.text.xml.simpleparser.SimpleXMLDocHandler;
import com.lowagie.text.xml.simpleparser.SimpleXMLParser;


public class SimplePatternParser implements SimpleXMLDocHandler,
        PatternConsumer {
    int currElement;

    PatternConsumer consumer;

    StringBuffer token;

    ArrayList exception;

    char hyphenChar;

    SimpleXMLParser parser;

    static final int ELEM_CLASSES = 1;

    static final int ELEM_EXCEPTIONS = 2;

    static final int ELEM_PATTERNS = 3;

    static final int ELEM_HYPHEN = 4;

    
    public SimplePatternParser() {
        token = new StringBuffer();
        hyphenChar = '-'; 
    }

    public void parse(InputStream stream, PatternConsumer consumer) {
        this.consumer = consumer;
        try {
            SimpleXMLParser.parse(this, stream);
        } catch (IOException e) {
            throw new ExceptionConverter(e);
        } finally {
            try {
                stream.close();
            } catch (Exception e) {
            }
        }
    }

    protected static String getPattern(String word) {
        StringBuffer pat = new StringBuffer();
        int len = word.length();
        for (int i = 0; i < len; i++) {
            if (!Character.isDigit(word.charAt(i))) {
                pat.append(word.charAt(i));
            }
        }
        return pat.toString();
    }

    protected ArrayList normalizeException(ArrayList ex) {
        ArrayList res = new ArrayList();
        for (int i = 0; i < ex.size(); i++) {
            Object item = ex.get(i);
            if (item instanceof String) {
                String str = (String) item;
                StringBuffer buf = new StringBuffer();
                for (int j = 0; j < str.length(); j++) {
                    char c = str.charAt(j);
                    if (c != hyphenChar) {
                        buf.append(c);
                    } else {
                        res.add(buf.toString());
                        buf.setLength(0);
                        char[] h = new char[1];
                        h[0] = hyphenChar;
                        
                        
                        res.add(new Hyphen(new String(h), null, null));
                    }
                }
                if (buf.length() > 0) {
                    res.add(buf.toString());
                }
            } else {
                res.add(item);
            }
        }
        return res;
    }

    protected String getExceptionWord(ArrayList ex) {
        StringBuffer res = new StringBuffer();
        for (int i = 0; i < ex.size(); i++) {
            Object item = ex.get(i);
            if (item instanceof String) {
                res.append((String) item);
            } else {
                if (((Hyphen) item).noBreak != null) {
                    res.append(((Hyphen) item).noBreak);
                }
            }
        }
        return res.toString();
    }

    protected static String getInterletterValues(String pat) {
        StringBuffer il = new StringBuffer();
        String word = pat + "a"; 
        int len = word.length();
        for (int i = 0; i < len; i++) {
            char c = word.charAt(i);
            if (Character.isDigit(c)) {
                il.append(c);
                i++;
            } else {
                il.append('0');
            }
        }
        return il.toString();
    }

    public void endDocument() {
    }

    public void endElement(String tag) {
        if (token.length() > 0) {
            String word = token.toString();
            switch (currElement) {
            case ELEM_CLASSES:
                consumer.addClass(word);
                break;
            case ELEM_EXCEPTIONS:
                exception.add(word);
                exception = normalizeException(exception);
                consumer.addException(getExceptionWord(exception),
                        (ArrayList) exception.clone());
                break;
            case ELEM_PATTERNS:
                consumer.addPattern(getPattern(word),
                        getInterletterValues(word));
                break;
            case ELEM_HYPHEN:
                
                break;
            }
            if (currElement != ELEM_HYPHEN) {
                token.setLength(0);
            }
        }
        if (currElement == ELEM_HYPHEN) {
            currElement = ELEM_EXCEPTIONS;
        } else {
            currElement = 0;
        }
    }

    public void startDocument() {
    }

    public void startElement(String tag, java.util.HashMap h) {
        if (tag.equals("hyphen-char")) {
            String hh = (String) h.get("value");
            if (hh != null && hh.length() == 1) {
                hyphenChar = hh.charAt(0);
            }
        } else if (tag.equals("classes")) {
            currElement = ELEM_CLASSES;
        } else if (tag.equals("patterns")) {
            currElement = ELEM_PATTERNS;
        } else if (tag.equals("exceptions")) {
            currElement = ELEM_EXCEPTIONS;
            exception = new ArrayList();
        } else if (tag.equals("hyphen")) {
            if (token.length() > 0) {
                exception.add(token.toString());
            }
            exception.add(new Hyphen((String) h.get(HtmlTags.PRE), (String) h
                    .get("no"), (String) h.get("post")));
            currElement = ELEM_HYPHEN;
        }
        token.setLength(0);
    }

    public void text(String str) {
        StringTokenizer tk = new StringTokenizer(str);
        while (tk.hasMoreTokens()) {
            String word = tk.nextToken();
            
            switch (currElement) {
            case ELEM_CLASSES:
                consumer.addClass(word);
                break;
            case ELEM_EXCEPTIONS:
                exception.add(word);
                exception = normalizeException(exception);
                consumer.addException(getExceptionWord(exception),
                        (ArrayList) exception.clone());
                exception.clear();
                break;
            case ELEM_PATTERNS:
                consumer.addPattern(getPattern(word),
                        getInterletterValues(word));
                break;
            }
        }
    }

    
    public void addClass(String c) {
        System.out.println("class: " + c);
    }

    public void addException(String w, ArrayList e) {
        System.out.println("exception: " + w + " : " + e.toString());
    }

    public void addPattern(String p, String v) {
        System.out.println("pattern: " + p + " : " + v);
    }

    public static void main(String[] args) throws Exception {
        try {
            if (args.length > 0) {
                SimplePatternParser pp = new SimplePatternParser();
                pp.parse(new FileInputStream(args[0]), pp);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
