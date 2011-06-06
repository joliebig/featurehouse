

package com.lowagie.text.xml;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.HashMap;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import com.lowagie.text.ExceptionConverter;



public class TagMap extends HashMap<String, XmlPeer> {

    private static final long serialVersionUID = -6809383366554350820L;

    class AttributeHandler extends DefaultHandler {
        

        public static final String TAG = "tag";
        

        public static final String ATTRIBUTE = "attribute";
        

        public static final String NAME = "name";
        

        public static final String ALIAS = "alias";
        

        public static final String VALUE = "value";
        

        public static final String CONTENT = "content";
        

        private HashMap<String, XmlPeer> tagMap;
        

        private XmlPeer currentPeer;
        

        
        public AttributeHandler(HashMap<String, XmlPeer> tagMap) {
            super();
            this.tagMap = tagMap;
        }
        

        
        public void startElement(String uri, String lname, String tag, Attributes attrs) {
            String name = attrs.getValue(NAME);
            String alias = attrs.getValue(ALIAS);
            String value = attrs.getValue(VALUE);
            if (name != null) {
                if(TAG.equals(tag)) {
                    currentPeer = new XmlPeer(name, alias);
                }
                else if (ATTRIBUTE.equals(tag)) {
                    if (alias != null) {
                        currentPeer.addAlias(name, alias);
                    }
                    if (value != null) {
                        currentPeer.addValue(name, value);
                    }
                }
            }
            value = attrs.getValue(CONTENT);
            if (value != null) {
                currentPeer.setContent(value);
            }
        }
        

        
        public void ignorableWhitespace(char[] ch, int start, int length) {
            
        }
        

        
        public void characters(char[] ch, int start, int length) {
            
        }
        

        
        public void endElement(String uri, String lname, String tag) {
            if (TAG.equals(tag))
                tagMap.put(currentPeer.getAlias(), currentPeer);
        }
    }
    
    
    public TagMap(String tagfile) {
        super();
        try {
            init(TagMap.class.getClassLoader().getResourceAsStream(tagfile));
        }catch(Exception e) {
            try {
                init(new FileInputStream(tagfile));
            } catch (FileNotFoundException fnfe) {
                throw new ExceptionConverter(fnfe);
            }
        }
    }

    
    public TagMap(InputStream in) {
        super();
        init(in);
    }

    protected void init(InputStream in) {
        try {
            SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
            parser.parse(new InputSource(in), new AttributeHandler(this));
        }
        catch(Exception e) {
            throw new ExceptionConverter(e);
        }
    }


}
