

package com.lowagie.text.html;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.lowagie.text.DocListener;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.xml.XmlParser;



public class HtmlParser extends XmlParser {
    

    
    public HtmlParser() {
        super();
    }
    

    
    public void go(DocListener document, InputSource is) {
        try {
            parser.parse(is, new SAXmyHtmlHandler(document));
        }
        catch(SAXException se) {
            throw new ExceptionConverter(se);
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    

    
    public static void parse(DocListener document, InputSource is) {
        HtmlParser p = new HtmlParser();
        p.go(document, is);
    }
    

    
    public void go(DocListener document, String file) {
        try {
            parser.parse(file, new SAXmyHtmlHandler(document));
        }
        catch(SAXException se) {
            throw new ExceptionConverter(se);
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    

    
    public static void parse(DocListener document, String file) {
        HtmlParser p = new HtmlParser();
        p.go(document, file);
    }
    

    
    public void go(DocListener document, InputStream is) {
        try {
            parser.parse(new InputSource(is), new SAXmyHtmlHandler(document));
        }
        catch(SAXException se) {
            throw new ExceptionConverter(se);
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    

    
    public static void parse(DocListener document, InputStream is) {
        HtmlParser p = new HtmlParser();
        p.go(document, new InputSource(is));
    }
    

    
    public void go(DocListener document, Reader is) {
        try {
            parser.parse(new InputSource(is), new SAXmyHtmlHandler(document));
        }
        catch(SAXException se) {
            throw new ExceptionConverter(se);
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    

    
    public static void parse(DocListener document, Reader is) {
        HtmlParser p = new HtmlParser();
        p.go(document, new InputSource(is));
    }
}