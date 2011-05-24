
package org.jhotdraw.contrib; 
import java.awt.*; 
public abstract  class  AutoscrollHelper {
		private int autoscrollMargin = 20;

		private Insets autoscrollInsets = new Insets(0, 0, 0, 0);

		public AutoscrollHelper(int margin) {	autoscrollMargin = margin;	}

		public void setAutoscrollMargin(int margin) {	autoscrollMargin = margin;	}

		public int getAutoscrollMargin() {	return autoscrollMargin;	}

		public abstract Dimension getSize();

		public abstract Rectangle getVisibleRect();

		public abstract void scrollRectToVisible(Rectangle aRect);

		public void autoscroll(Point location) {	int top = 0, left = 0, bottom = 0, right = 0;	Dimension size = getSize();	Rectangle rect = getVisibleRect();	int bottomEdge = rect.y + rect.height;	int rightEdge = rect.x + rect.width;	if (location.y - rect.y <= autoscrollMargin && rect.y > 0)	top = autoscrollMargin;	if (location.x - rect.x <= autoscrollMargin && rect.x > 0)	left = autoscrollMargin;	if (bottomEdge - location.y <= autoscrollMargin && bottomEdge < size.height)	bottom = autoscrollMargin;	if (rightEdge - location.x <= autoscrollMargin && rightEdge < size.width)	right = autoscrollMargin;	rect.x += right - left;	rect.y += bottom - top;	scrollRectToVisible(rect);	}

		public Insets getAutoscrollInsets() {	Dimension size = getSize();	Rectangle rect = getVisibleRect();	autoscrollInsets.top = rect.y + autoscrollMargin;	autoscrollInsets.left = rect.x + autoscrollMargin;	autoscrollInsets.bottom = size.height - (rect.y + rect.height) + autoscrollMargin;	autoscrollInsets.right = size.width - (rect.x + rect.width) + autoscrollMargin;	return autoscrollInsets;	}


}
