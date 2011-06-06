

package com.lowagie.text.xml.xmp;



public class DublinCoreSchema extends XmpSchema {

    private static final long serialVersionUID = -4551741356374797330L;
    
    public static final String DEFAULT_XPATH_ID = "dc";
    
    public static final String DEFAULT_XPATH_URI = "http://purl.org/dc/elements/1.1/";
    
    
    public static final String CONTRIBUTOR = "dc:contributor";
    
    public static final String COVERAGE = "dc:coverage";
    
    public static final String CREATOR = "dc:creator";
    
    public static final String DATE = "dc:date";
    
    public static final String DESCRIPTION = "dc:description";
    
    public static final String FORMAT = "dc:format";
    
    public static final String IDENTIFIER = "dc:identifier";
    
    public static final String LANGUAGE = "dc:language";
    
    public static final String PUBLISHER = "dc:publisher";
    
    public static final String RELATION = "dc:relation";
    
    public static final String RIGHTS = "dc:rights";
    
    public static final String SOURCE = "dc:source";
    
    public static final String SUBJECT = "dc:subject";
    
    public static final String TITLE = "dc:title";
    
    public static final String TYPE = "dc:type";

    
    public DublinCoreSchema() {
        super("xmlns:" + DEFAULT_XPATH_ID + "=\"" + DEFAULT_XPATH_URI + "\"");
        setProperty(FORMAT, "application/pdf");
    }
    
    
    public void addTitle(String title) {
        setProperty(TITLE, title);
    }

    
    public void addDescription(String desc) {
        setProperty(DESCRIPTION, desc);
    }

    
    public void addSubject(String subject) {
        XmpArray array = new XmpArray(XmpArray.UNORDERED);
        array.add(subject);
        setProperty(SUBJECT, array);
    }

    
    
    public void addSubject(String[] subject) {
        XmpArray array = new XmpArray(XmpArray.UNORDERED);
        for (int i = 0; i < subject.length; i++) {
            array.add(subject[i]);
        }
        setProperty(SUBJECT, array);
    }
    
    
    public void addAuthor(String author) {
        XmpArray array = new XmpArray(XmpArray.ORDERED);
        array.add(author);
        setProperty(CREATOR, array);
    }

    
    public void addAuthor(String[] author) {
        XmpArray array = new XmpArray(XmpArray.ORDERED);
        for (int i = 0; i < author.length; i++) {
            array.add(author[i]);
        }
        setProperty(CREATOR, array);
    }

    
    public void addPublisher(String publisher) {
        XmpArray array = new XmpArray(XmpArray.ORDERED);
        array.add(publisher);
        setProperty(PUBLISHER, array);
    }

    
    public void addPublisher(String[] publisher) {
        XmpArray array = new XmpArray(XmpArray.ORDERED);
        for (int i = 0; i < publisher.length; i++) {
            array.add(publisher[i]);
        }
        setProperty(PUBLISHER, array);
    }
}
