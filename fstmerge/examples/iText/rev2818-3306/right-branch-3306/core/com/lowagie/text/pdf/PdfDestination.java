

package com.lowagie.text.pdf;



public class PdfDestination extends PdfArray {
    
    
    

    public static final int XYZ = 0;
    

    public static final int FIT = 1;
    

    public static final int FITH = 2;
    

    public static final int FITV = 3;
    

    public static final int FITR = 4;
    

    public static final int FITB = 5;
    

    public static final int FITBH = 6;
    

    public static final int FITBV = 7;
    
    
    

    private boolean status = false;
    
    
    

    
    public PdfDestination(int type) {
        super();
        if (type == FITB) {
            add(PdfName.FITB);
        }
        else {
            add(PdfName.FIT);
        }
    }
    

    
    public PdfDestination(int type, float parameter) {
        super(new PdfNumber(parameter));
        switch(type) {
            default:
                addFirst(PdfName.FITH);
                break;
            case FITV:
                addFirst(PdfName.FITV);
                break;
            case FITBH:
                addFirst(PdfName.FITBH);
                break;
            case FITBV:
                addFirst(PdfName.FITBV);
        }
    }
    

    
    public PdfDestination(int type, float left, float top, float zoom) {
        super(PdfName.XYZ);
        if (left < 0)
            add(PdfNull.PDFNULL);
        else
            add(new PdfNumber(left));
        if (top < 0)
            add(PdfNull.PDFNULL);
        else
            add(new PdfNumber(top));
        add(new PdfNumber(zoom));
    }
    

    
    public PdfDestination(int type, float left, float bottom, float right, float top) {
        super(PdfName.FITR);
        add(new PdfNumber(left));
        add(new PdfNumber(bottom));
        add(new PdfNumber(right));
        add(new PdfNumber(top));
    }
    
    
    

    
    public boolean hasPage() {
        return status;
    }
    

    
    public boolean addPage(PdfIndirectReference page) {
        if (!status) {
            addFirst(page);
            status = true;
            return true;
        }
        return false;
    }
}