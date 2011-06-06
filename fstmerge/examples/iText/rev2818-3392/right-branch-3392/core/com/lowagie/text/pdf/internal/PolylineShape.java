

package com.lowagie.text.pdf.internal;

import java.awt.Shape;
import java.awt.Rectangle;
import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;


public class PolylineShape implements Shape {
    
    protected int[] x;
    
    protected int[] y;
    
    protected int np;

    
    public PolylineShape(int[] x, int[] y, int nPoints) {
        
        this.np = nPoints;
        
        this.x = new int[np];
        this.y = new int[np];
        System.arraycopy(x, 0, this.x, 0, np);
        System.arraycopy(y, 0, this.y, 0, np);
    }

    
    public Rectangle2D getBounds2D() {
        int[] r = rect();
        return r==null?null:new Rectangle2D.Double(r[0], r[1], r[2], r[3]);
    }
    
    
    public Rectangle getBounds() {
        return getBounds2D().getBounds();
    }

    
    private int[] rect() {
         if(np==0)return null;
        int xMin = x[0], yMin=y[0], xMax=x[0],yMax=y[0];

         for(int i=1;i<np;i++) {
             if(x[i]<xMin)xMin=x[i];
             else if(x[i]>xMax)xMax=x[i];
             if(y[i]<yMin)yMin=y[i];
             else if(y[i]>yMax)yMax=y[i];
         }

         return new int[] { xMin, yMin, xMax-xMin, yMax-yMin };
    }

    
    public boolean contains(double x, double y) { return false; }
    
    
    public boolean contains(Point2D p) { return false; }
    
    
    public boolean contains(double x, double y, double w, double h) { return false; }
    
    
    public boolean contains(Rectangle2D r) { return false; }

    
    public boolean intersects(double x, double y, double w, double h) {
        return intersects(new Rectangle2D.Double(x, y, w, h));
    }

    
    public boolean intersects(Rectangle2D r) {
        if(np==0)return false;
        Line2D line = new Line2D.Double(x[0],y[0],x[0],y[0]);
        for (int i = 1; i < np; i++) {
            line.setLine(x[i-1], y[i-1], x[i], y[i]);
            if(line.intersects(r))return true;
        }
        return false;
    }

    
    public PathIterator getPathIterator(AffineTransform at) {
        return new PolylineShapeIterator(this, at);
    }

    
    public PathIterator getPathIterator(AffineTransform at, double flatness) {
        return new PolylineShapeIterator(this, at);
    }

}

