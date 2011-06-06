

package com.lowagie.text.pdf;

import java.lang.ref.WeakReference;



public class PdfIndirectReference extends PdfObject {

    


    protected int number;


    protected int generation = 0;

    
    protected WeakReference reffedObj;

    

    protected PdfIndirectReference() {
        super(0);
    }



    PdfIndirectReference(int type, int number, int generation) {
        super(0, new StringBuffer().append(number).append(" ").append(generation).append(" R").toString());
        this.number = number;
        this.generation = generation;
    }



    PdfIndirectReference(int type, int number) {
        this(type, number, 0);
    }

    



    public int getNumber() {
        return number;
    }



    public int getGeneration() {
        return generation;
    }

    public String toString() {
        return new StringBuffer().append(number).append(" ").append(generation).append(" R").toString();
    }

    
    public void setDirectObject( PdfObject obj ) {
        reffedObj = new WeakReference( obj );
    }

    
    public PdfObject getDirectObject() {
        if (reffedObj != null) {
            return (PdfObject) reffedObj.get();
        }
        return null;
    }
}