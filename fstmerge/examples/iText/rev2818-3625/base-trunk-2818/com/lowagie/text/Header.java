

package com.lowagie.text;



public class Header extends Meta {
    
    
    
    
    private StringBuffer name;
    
    
    
    
    
    public Header(String name, String content) {
        super(Element.HEADER, content);
        this.name = new StringBuffer(name);
    }
    
    

    
    public String getName() {
        return name.toString();
    }
    
    
    
    
    public String name() {
        return getName();
    }
}