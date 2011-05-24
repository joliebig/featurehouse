
package org.jhotdraw.figures; 
import java.awt.Polygon; 
public  class  ArrowTip  extends AbstractLineDecoration {
		private double fAngle;

		private double fOuterRadius;

		private double fInnerRadius;

		private static final long serialVersionUID = -3459171428373823638L;

		private int arrowTipSerializedDataVersion = 1;

		public ArrowTip() {	this(0.40, 8, 8);	}

		public ArrowTip(double angle, double outerRadius, double innerRadius) {	setAngle(angle);	setOuterRadius(outerRadius);	setInnerRadius(innerRadius);	}

		public Polygon outline(int x1, int y1, int x2, int y2) {	double dir = Math.PI/2 - Math.atan2(x2 - x1, y2 - y1);	return outline(x1, y1, dir);	}

		private Polygon outline(int x, int y, double direction) {	Polygon shape = new Polygon();	shape.addPoint(x, y);	addPointRelative(shape, x, y, getOuterRadius(), direction - getAngle());	addPointRelative(shape, x, y, getInnerRadius(), direction);	addPointRelative(shape, x, y, getOuterRadius(), direction + getAngle());	shape.addPoint(x,y);	return shape;	}

		private void addPointRelative(Polygon shape, int x, int y, double radius, double angle) {	shape.addPoint(	x + (int) (radius * Math.cos(angle)),	y + (int) (radius * Math.sin(angle)));	}

		protected void setAngle(double newAngle) {	fAngle = newAngle;	}

		protected double getAngle() {	return fAngle;	}

		protected void setInnerRadius(double newInnerRadius) {	fInnerRadius = newInnerRadius;	}

		protected double getInnerRadius() {	return fInnerRadius;	}

		protected void setOuterRadius(double newOuterRadius) {	fOuterRadius = newOuterRadius;	}

		protected double getOuterRadius() {	return fOuterRadius;	}


}
