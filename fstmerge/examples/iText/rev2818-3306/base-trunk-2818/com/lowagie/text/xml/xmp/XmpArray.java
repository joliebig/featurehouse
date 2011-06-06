

package com.lowagie.text.xml.xmp;

import java.util.ArrayList;
import java.util.Iterator;


public class XmpArray extends ArrayList {

    private static final long serialVersionUID = 5722854116328732742L;
    
    public static final String UNORDERED = "rdf:Bag";
    
    public static final String ORDERED = "rdf:Seq";
    
    public static final String ALTERNATIVE = "rdf:Alt";
    
    
    protected String type;
    
    
    public XmpArray(String type) {
        this.type = type;
    }
    
    
    public String toString() {
        StringBuffer buf = new StringBuffer("<");
        buf.append(type);
        buf.append('>');
        String s;
        for (Iterator i = iterator(); i.hasNext(); ) {
            s = (String) i.next();
            buf.append("<rdf:li>");
            buf.append(XmpSchema.escape(s));
            buf.append("</rdf:li>");
        }
        buf.append("</");
        buf.append(type);
        buf.append('>');
        return buf.toString();
    }
}