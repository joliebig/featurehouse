

package com.lowagie.text.html;

import java.util.Properties;

import org.xml.sax.Attributes;

import com.lowagie.text.ElementTags;
import com.lowagie.text.xml.XmlPeer;



public class HtmlPeer extends XmlPeer {

    

    public HtmlPeer(String name, String alias) {
        super(name, alias.toLowerCase());
    }

    

    public void addAlias(String name, String alias) {
        attributeAliases.put(alias.toLowerCase(), name);
    }

    
    public Properties getAttributes(Attributes attrs) {
        Properties attributes = new Properties();
        attributes.putAll(attributeValues);
        if (defaultContent != null) {
            attributes.put(ElementTags.ITEXT, defaultContent);
        }
        if (attrs != null) {
            String attribute, value;
            for (int i = 0; i < attrs.getLength(); i++) {
                attribute = getName(attrs.getQName(i).toLowerCase());
                value = attrs.getValue(i);
                attributes.setProperty(attribute, value);
            }
        }
        return attributes;
    }
}

