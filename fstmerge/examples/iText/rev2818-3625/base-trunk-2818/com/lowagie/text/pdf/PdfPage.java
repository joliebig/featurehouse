

package com.lowagie.text.pdf;
import java.util.HashMap;


public class PdfPage extends PdfDictionary {

    private static final String boxStrings[] = {"crop", "trim", "art", "bleed"};
    private static final PdfName boxNames[] = {PdfName.CROPBOX, PdfName.TRIMBOX, PdfName.ARTBOX, PdfName.BLEEDBOX};
    
    

    public static final PdfNumber PORTRAIT = new PdfNumber(0);
    

    public static final PdfNumber LANDSCAPE = new PdfNumber(90);
    

    public static final PdfNumber INVERTEDPORTRAIT = new PdfNumber(180);
    

    public static final PdfNumber SEASCAPE = new PdfNumber(270);
    

    PdfRectangle mediaBox;
    
    
    

    











    

    
    PdfPage(PdfRectangle mediaBox, HashMap boxSize, PdfDictionary resources, int rotate) {
        super(PAGE);
        this.mediaBox = mediaBox;
        put(PdfName.MEDIABOX, mediaBox);
        put(PdfName.RESOURCES, resources);
        if (rotate != 0) {
            put(PdfName.ROTATE, new PdfNumber(rotate));
        }
        for (int k = 0; k < boxStrings.length; ++k) {
            PdfObject rect = (PdfObject)boxSize.get(boxStrings[k]);
            if (rect != null)
                put(boxNames[k], rect);
        }
    }
    

    



    

    
    PdfPage(PdfRectangle mediaBox, HashMap boxSize, PdfDictionary resources) {
        this(mediaBox, boxSize, resources, 0);
    }
    

    
    public boolean isParent() {
        return false;
    }
    
    
    

    
    void add(PdfIndirectReference contents) {
        put(PdfName.CONTENTS, contents);
    }
    

    
    PdfRectangle rotateMediaBox() {
        this.mediaBox =  mediaBox.rotate();
        put(PdfName.MEDIABOX, this.mediaBox);
        return this.mediaBox;
    }
    

    
    PdfRectangle getMediaBox() {
        return mediaBox;
    }
}