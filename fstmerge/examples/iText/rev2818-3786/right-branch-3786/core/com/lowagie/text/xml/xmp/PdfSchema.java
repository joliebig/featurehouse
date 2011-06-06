

package com.lowagie.text.xml.xmp;

import com.lowagie.text.Document;


public class PdfSchema extends XmpSchema {

    private static final long serialVersionUID = -1541148669123992185L;
    
    public static final String DEFAULT_XPATH_ID = "pdf";
    
    public static final String DEFAULT_XPATH_URI = "http://ns.adobe.com/pdf/1.3/";
    
    
    public static final String KEYWORDS = "pdf:Keywords";
    
    public static final String VERSION = "pdf:PDFVersion";
    
    public static final String PRODUCER = "pdf:Producer";


    public PdfSchema() {
        super("xmlns:" + DEFAULT_XPATH_ID + "=\"" + DEFAULT_XPATH_URI + "\"");
        addProducer(Document.getVersion());
    }
    
    
    public void addKeywords(String keywords) {
        setProperty(KEYWORDS, keywords);
    }
    
    
    public void addProducer(String producer) {
        setProperty(PRODUCER, producer);
    }

    
    public void addVersion(String version) {
        setProperty(VERSION, version);
    }
}
