

package com.lowagie.text.xml;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.HashMap;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.lowagie.text.DocListener;
import com.lowagie.text.ExceptionConverter;



public class XmlParser {
    

    protected SAXParser parser;
    

    
    public XmlParser() {
        try {
            parser = SAXParserFactory.newInstance().newSAXParser();
        }
        catch(ParserConfigurationException pce) {
            throw new ExceptionConverter(pce);
        }
        catch(SAXException se) {
            throw new ExceptionConverter(se);
        }
    }
    

    
    public void go(DocListener document, InputSource is) {
        try {
            parser.parse(is, new SAXiTextHandler(document));
        }
        catch(SAXException se) {
            throw new ExceptionConverter(se);
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    

    
    public void go(DocListener document, InputSource is, String tagmap) {
        try {
            parser.parse(is, new SAXmyHandler(document, new TagMap(tagmap)));
        }
        catch(SAXException se) {
            throw new ExceptionConverter(se);
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    
    
        
        public void go(DocListener document, InputSource is, InputStream tagmap) {
            try {
                parser.parse(is, new SAXmyHandler(document, new TagMap(tagmap)));
            }
            catch(SAXException se) {
                throw new ExceptionConverter(se);
            }
            catch(IOException ioe) {
                throw new ExceptionConverter(ioe);
            }
        }
    

    
    public void go(DocListener document, InputSource is, HashMap tagmap) {
        try {
            parser.parse(is, new SAXmyHandler(document, tagmap));
        }
        catch(SAXException se) {
            throw new ExceptionConverter(se);
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    

    
    public void go(DocListener document, String file) {
        try {
            parser.parse(file, new SAXiTextHandler(document));
        }
        catch(SAXException se) {
            throw new ExceptionConverter(se);
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    

    
    public void go(DocListener document, String file, String tagmap) {
        try {
            parser.parse(file, new SAXmyHandler(document, new TagMap(tagmap)));
        }
        catch(SAXException se) {
            throw new ExceptionConverter(se);
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    

    
    public void go(DocListener document, String file, HashMap tagmap) {
        try {
            parser.parse(file, new SAXmyHandler(document, tagmap));
        }
        catch(SAXException se) {
            throw new ExceptionConverter(se);
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    

    
    public static void parse(DocListener document, InputSource is) {
        XmlParser p = new XmlParser();
        p.go(document, is);
    }
    

    
    public static void parse(DocListener document, InputSource is, String tagmap) {
        XmlParser p = new XmlParser();
        p.go(document, is, tagmap);
    }
    

    
    public static void parse(DocListener document, InputSource is, HashMap tagmap) {
        XmlParser p = new XmlParser();
        p.go(document, is, tagmap);
    }
    

    
    public static void parse(DocListener document, String file) {
        XmlParser p = new XmlParser();
        p.go(document, file);
    }
    

    
    public static void parse(DocListener document, String file, String tagmap) {
        XmlParser p = new XmlParser();
        p.go(document, file, tagmap);
    }
    

    
    public static void parse(DocListener document, String file, HashMap tagmap) {
        XmlParser p = new XmlParser();
        p.go(document, file, tagmap);
    }
    

    
    public static void parse(DocListener document, InputStream is) {
        XmlParser p = new XmlParser();
        p.go(document, new InputSource(is));
    }
    

    
    public static void parse(DocListener document, InputStream is, String tagmap) {
        XmlParser p = new XmlParser();
        p.go(document, new InputSource(is), tagmap);
    }
    

    
    public static void parse(DocListener document, InputStream is, HashMap tagmap) {
        XmlParser p = new XmlParser();
        p.go(document, new InputSource(is), tagmap);
    }
    

    
    public static void parse(DocListener document, Reader is) {
        XmlParser p = new XmlParser();
        p.go(document, new InputSource(is));
    }
    

    
    public static void parse(DocListener document, Reader is, String tagmap) {
        XmlParser p = new XmlParser();
        p.go(document, new InputSource(is), tagmap);
    }
    

    
    public static void parse(DocListener document, Reader is, HashMap tagmap) {
        XmlParser p = new XmlParser();
        p.go(document, new InputSource(is), tagmap);
    }
}