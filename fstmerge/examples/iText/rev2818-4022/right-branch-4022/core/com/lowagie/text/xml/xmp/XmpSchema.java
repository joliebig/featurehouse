

package com.lowagie.text.xml.xmp;

import java.util.Enumeration;
import java.util.Properties;


public abstract class XmpSchema extends Properties {

    private static final long serialVersionUID = -176374295948945272L;

    
    protected String xmlns;
    
    
    public XmpSchema(String xmlns) {
        super();
        this.xmlns = xmlns;
    }
    
    public String toString() {
        StringBuffer buf = new StringBuffer();
        for (Enumeration<?> e = this.propertyNames(); e.hasMoreElements(); ) {
            process(buf, e.nextElement());
        }
        return buf.toString();
    }
    
    protected void process(StringBuffer buf, Object p) {
        buf.append('<');
        buf.append(p);
        buf.append('>');
        buf.append(this.get(p));
        buf.append("</");
        buf.append(p);
        buf.append('>');
    }
    
    public String getXmlns() {
        return xmlns;
    }    
    
    
    public Object addProperty(String key, String value) {
        return this.setProperty(key, value);
    }
    
    
    public Object setProperty(String key, String value) {
        return super.setProperty(key, escape(value));
    }
    
    
    public Object setProperty(String key, XmpArray value) {
        return super.setProperty(key, value.toString());
    }
    
    
    public Object setProperty(String key, LangAlt value) {
        return super.setProperty(key, value.toString());
     }
    
    
    public static String escape(String content) {
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < content.length(); i++) {
            switch(content.charAt(i)) {
            case '<':
                buf.append("&lt;");
                break;
            case '>':
                buf.append("&gt;");
                break;
            case '\'':
                buf.append("&apos;");
                break;
            case '\"':
                buf.append("&quot;");
                break;
            case '&':
                buf.append("&amp;");
                break;
            default:
                buf.append(content.charAt(i));
            }
        }
        return buf.toString();
    }
}
