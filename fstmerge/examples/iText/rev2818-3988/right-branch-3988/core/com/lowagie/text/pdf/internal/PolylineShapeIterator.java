

package com.lowagie.text.pdf.internal;

import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;
import java.util.NoSuchElementException;

public class PolylineShapeIterator implements PathIterator {
    
    protected PolylineShape poly;
    
    protected AffineTransform affine;
    
    protected int index;
    
    
    PolylineShapeIterator(PolylineShape l, AffineTransform at) {
        this.poly = l;
        this.affine = at;
    }
    
    
    public int currentSegment(double[] coords) {
        if (isDone()) {
            throw new NoSuchElementException("line iterator out of bounds");
        }
        int type = (index==0)?SEG_MOVETO:SEG_LINETO;
        coords[0] = poly.x[index];
        coords[1] = poly.y[index];
        if (affine != null) {
            affine.transform(coords, 0, coords, 0, 1);
        }
        return type;
    }
    
    
    public int currentSegment(float[] coords) {
        if (isDone()) {
            throw new NoSuchElementException("line iterator out of bounds");
        }
        int type = (index==0)?SEG_MOVETO:SEG_LINETO;
        coords[0] = poly.x[index];
        coords[1] = poly.y[index];
        if (affine != null) {
            affine.transform(coords, 0, coords, 0, 1);
        }
        return type;
    }

    
    public int getWindingRule() {
        return WIND_NON_ZERO;
    }

    
    public boolean isDone() {
        return (index >= poly.np);
    }

    
    public void next() {
        index++;
    }

}
