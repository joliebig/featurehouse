

package com.lowagie.text.xml.xmp;


public class PdfA1Schema extends XmpSchema {
    
    
    public static final String DEFAULT_XPATH_ID = "pdfaid";
    
    public static final String DEFAULT_XPATH_URI = "http://www.aiim.org/pdfa/ns/id/";
    
    
    public static final String PART = "pdfaid:part";
    
    public static final String CONFORMANCE = "pdfaid:conformance";
    
    public PdfA1Schema() {
        super("xmlns:" + DEFAULT_XPATH_ID + "=\"" + DEFAULT_XPATH_URI + "\"");
        addPart("1");
    }
    
    
    public void addPart(String part) {
        setProperty(PART, part);
    }
    
    
    public void addConformance(String conformance) {
        setProperty(CONFORMANCE, conformance);
    }
}