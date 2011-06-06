

package com.lowagie.text;

import java.util.ArrayList;
import java.util.Properties;



public class MarkedObject implements Element {

    
    protected Element element;

    
    protected Properties markupAttributes = new Properties();
        
    
    protected MarkedObject() {
        element = null;
    }
    
    
    public MarkedObject(Element element) {
        this.element = element;
    }
    
    
    public ArrayList<Chunk> getChunks() {
        return element.getChunks();
    }

    
    public boolean process(ElementListener listener) {
        try {
            return listener.add(element);
        }
        catch(DocumentException de) {
            return false;
        }
    }
    
    
    public int type() {
        return MARKED;
    }
    
    
    public boolean isContent() {
        return true;
    }

    
    public boolean isNestable() {
        return true;
    }

    
    public Properties getMarkupAttributes() {
        return markupAttributes;
    }
    
    
    public void setMarkupAttribute(String key, String value) {
        markupAttributes.setProperty(key, value);
    }

}