

package com.lowagie.text;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;



public class Annotation implements Element {

    

    
    public static final int TEXT = 0;

    
    public static final int URL_NET = 1;

    
    public static final int URL_AS_STRING = 2;

    
    public static final int FILE_DEST = 3;

    
    public static final int FILE_PAGE = 4;

    
    public static final int NAMED_DEST = 5;

    
    public static final int LAUNCH = 6;

    
    public static final int SCREEN = 7;

    
    public static final String TITLE = "title";

    
    public static final String CONTENT = "content";

    
    public static final String URL = "url";

    
    public static final String FILE = "file";

    
    public static final String DESTINATION = "destination";

    
    public static final String PAGE = "page";

    
    public static final String NAMED = "named";

    
    public static final String APPLICATION = "application";

    
    public static final String PARAMETERS = "parameters";

    
    public static final String OPERATION = "operation";

    
    public static final String DEFAULTDIR = "defaultdir";

    
    public static final String LLX = "llx";

    
    public static final String LLY = "lly";

    
    public static final String URX = "urx";

    
    public static final String URY = "ury";

    
    public static final String MIMETYPE = "mime";

    
    protected int annotationtype;

    
    protected HashMap<String, Object> annotationAttributes = new HashMap<String, Object>();

    
    protected float llx = Float.NaN;

    
    protected float lly = Float.NaN;

    
    protected float urx = Float.NaN;

    
    protected float ury = Float.NaN;

    

    
    private Annotation(float llx, float lly, float urx, float ury) {
        this.llx = llx;
        this.lly = lly;
        this.urx = urx;
        this.ury = ury;
    }

    
    public Annotation(Annotation an) {
        annotationtype = an.annotationtype;
        annotationAttributes = an.annotationAttributes;
        llx = an.llx;
        lly = an.lly;
        urx = an.urx;
        ury = an.ury;
    }
    
    
    public Annotation(String title, String text) {
        annotationtype = TEXT;
        annotationAttributes.put(TITLE, title);
        annotationAttributes.put(CONTENT, text);
    }

    
    public Annotation(String title, String text, float llx, float lly,
            float urx, float ury) {
        this(llx, lly, urx, ury);
        annotationtype = TEXT;
        annotationAttributes.put(TITLE, title);
        annotationAttributes.put(CONTENT, text);
    }

    
    public Annotation(float llx, float lly, float urx, float ury, URL url) {
        this(llx, lly, urx, ury);
        annotationtype = URL_NET;
        annotationAttributes.put(URL, url);
    }

    
    public Annotation(float llx, float lly, float urx, float ury, String url) {
        this(llx, lly, urx, ury);
        annotationtype = URL_AS_STRING;
        annotationAttributes.put(FILE, url);
    }

    
    public Annotation(float llx, float lly, float urx, float ury, String file,
            String dest) {
        this(llx, lly, urx, ury);
        annotationtype = FILE_DEST;
        annotationAttributes.put(FILE, file);
        annotationAttributes.put(DESTINATION, dest);
    }

    
    public Annotation(float llx, float lly, float urx, float ury,
            String moviePath, String mimeType, boolean showOnDisplay) {
        this(llx, lly, urx, ury);
        annotationtype = SCREEN;
        annotationAttributes.put(FILE, moviePath);
        annotationAttributes.put(MIMETYPE, mimeType);
        annotationAttributes.put(PARAMETERS, new boolean[] {
                false , showOnDisplay });
    }

    
    public Annotation(float llx, float lly, float urx, float ury, String file,
            int page) {
        this(llx, lly, urx, ury);
        annotationtype = FILE_PAGE;
        annotationAttributes.put(FILE, file);
        annotationAttributes.put(PAGE, new Integer(page));
    }

    
    public Annotation(float llx, float lly, float urx, float ury, int named) {
        this(llx, lly, urx, ury);
        annotationtype = NAMED_DEST;
        annotationAttributes.put(NAMED, new Integer(named));
    }

    
    public Annotation(float llx, float lly, float urx, float ury,
            String application, String parameters, String operation,
            String defaultdir) {
        this(llx, lly, urx, ury);
        annotationtype = LAUNCH;
        annotationAttributes.put(APPLICATION, application);
        annotationAttributes.put(PARAMETERS, parameters);
        annotationAttributes.put(OPERATION, operation);
        annotationAttributes.put(DEFAULTDIR, defaultdir);
    }

    

    
    public int type() {
        return Element.ANNOTATION;
    }

    
    public boolean process(ElementListener listener) {
        try {
            return listener.add(this);
        } catch (DocumentException de) {
            return false;
        }
    }

    

    public ArrayList<Chunk> getChunks() {
        return new ArrayList<Chunk>();
    }

    

    
    public void setDimensions(float llx, float lly, float urx, float ury) {
        this.llx = llx;
        this.lly = lly;
        this.urx = urx;
        this.ury = ury;
    }

    

    
    public float llx() {
        return llx;
    }

    
    public float lly() {
        return lly;
    }

    
    public float urx() {
        return urx;
    }

    
    public float ury() {
        return ury;
    }

    
    public float llx(float def) {
        if (Float.isNaN(llx))
            return def;
        return llx;
    }

    
    public float lly(float def) {
        if (Float.isNaN(lly))
            return def;
        return lly;
    }

    
    public float urx(float def) {
        if (Float.isNaN(urx))
            return def;
        return urx;
    }

    
    public float ury(float def) {
        if (Float.isNaN(ury))
            return def;
        return ury;
    }

    
    public int annotationType() {
        return annotationtype;
    }

    
    public String title() {
        String s = (String) annotationAttributes.get(TITLE);
        if (s == null)
            s = "";
        return s;
    }

    
    public String content() {
        String s = (String) annotationAttributes.get(CONTENT);
        if (s == null)
            s = "";
        return s;
    }

    
    public HashMap<String, Object> attributes() {
        return annotationAttributes;
    }

    
    public Annotation(java.util.Properties attributes) {
        this(com.lowagie.text.factories.ElementFactory.getAnnotation(attributes));
    }
}