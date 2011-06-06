

package com.lowagie.text.xml.xmp;

import java.util.Enumeration;
import java.util.Properties;

public class LangAlt extends Properties {

    
    private static final long serialVersionUID = 4396971487200843099L;
    
    
    public static final String DEFAULT = "x-default";

    
    public LangAlt(String defaultValue) {
        super();
        addLanguage(DEFAULT, defaultValue);
    }

    
    public LangAlt() {
        super();
    }

    
    public void addLanguage(String language, String value) {
        setProperty(language, XmpSchema.escape(value));
    }

    
    protected void process(StringBuffer buf, Object lang) {
        buf.append("<rdf:li xml:lang=\"");
        buf.append(lang);
        buf.append("\" >");
        buf.append(get(lang));
        buf.append("</rdf:li>");
    }

    
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("<rdf:Alt>");
        for (Enumeration<?> e = this.propertyNames(); e.hasMoreElements();) {
            process(sb, e.nextElement());
        }
        sb.append("</rdf:Alt>");
        return sb.toString();
    }

}