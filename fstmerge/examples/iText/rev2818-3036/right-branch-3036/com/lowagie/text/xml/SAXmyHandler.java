

package com.lowagie.text.xml;

import java.util.HashMap;
import java.util.Properties;

import org.xml.sax.Attributes;

import com.lowagie.text.DocListener;



public class SAXmyHandler extends SAXiTextHandler {
    

    protected HashMap<String, XmlPeer> myTags;
    

    
    public SAXmyHandler(DocListener document, HashMap<String, XmlPeer> myTags) {
        super(document);
        this.myTags = myTags;
    }
    

    
    public void startElement(String uri, String lname, String name, Attributes attrs) {
        if (myTags.containsKey(name)) {
            XmlPeer peer = myTags.get(name);
            handleStartingTags(peer.getTag(), peer.getAttributes(attrs));
        }
        else {
            Properties attributes = new Properties();
            if (attrs != null) {
                for (int i = 0; i < attrs.getLength(); i++) {
                    String attribute = attrs.getQName(i);
                    attributes.setProperty(attribute, attrs.getValue(i));
                }
            }
            handleStartingTags(name, attributes);
        }
    }
    
    
    
    public void endElement(String uri, String lname, String name) {
        if (myTags.containsKey(name)) {
            XmlPeer peer = myTags.get(name);
            handleEndingTags(peer.getTag());
        }
        else {
            handleEndingTags(name);
        }
    }
}