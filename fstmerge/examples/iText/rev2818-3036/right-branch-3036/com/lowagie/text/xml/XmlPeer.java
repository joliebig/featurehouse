

package com.lowagie.text.xml;

import java.util.Properties;

import org.xml.sax.Attributes;

import com.lowagie.text.ElementTags;



public class XmlPeer {
    

    protected String tagname;
    

    protected String customTagname;
    

    protected Properties attributeAliases = new Properties();
    

    protected Properties attributeValues = new Properties();
    

    protected String defaultContent = null;
    

    
    public XmlPeer(String name, String alias) {
        this.tagname = name;
        this.customTagname = alias;
    }
    

    
    public String getTag() {
        return tagname;
    }
    

    
    public String getAlias() {
        return customTagname;
    }
    

    public Properties getAttributes(Attributes attrs) {
        Properties attributes = new Properties();
        attributes.putAll(attributeValues);
        if (defaultContent != null) {
            attributes.put(ElementTags.ITEXT, defaultContent);
        }
        if (attrs != null) {
            for (int i = 0; i < attrs.getLength(); i++) {
                String attribute = getName(attrs.getQName(i));
                attributes.setProperty(attribute, attrs.getValue(i));
            }
        }
        return attributes;
    }
    

    
    public void addAlias(String name, String alias) {
        attributeAliases.put(alias, name);
    }
    

    
    public void addValue(String name, String value) {
        attributeValues.put(name, value);
    }
    

    
    public void setContent(String content) {
        this.defaultContent = content;
    }
    

    
    public String getName(String name) {
        String value;
        if ((value = attributeAliases.getProperty(name)) != null) {
            return value;
        }
        return name;
    }
    

    
    public Properties getDefaultValues() {
        return attributeValues;
    }
}