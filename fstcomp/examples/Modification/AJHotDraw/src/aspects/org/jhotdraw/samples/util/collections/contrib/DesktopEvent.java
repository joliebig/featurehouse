
package org.jhotdraw.contrib; 
import org.jhotdraw.framework.DrawingView; 
import java.util.EventObject; 
public  class  DesktopEvent  extends EventObject {
		private DrawingView myDrawingView;

		private DrawingView myPreviousDrawingView;

		public DesktopEvent(Desktop newSource, DrawingView newDrawingView) {	this(newSource, newDrawingView, null);	}

		public DesktopEvent(Desktop newSource, DrawingView newDrawingView, DrawingView newPreviousDV) {	super(newSource);	setDrawingView(newDrawingView);	setPreviousDrawingView(newPreviousDV);	}

		private void setDrawingView(DrawingView newDrawingView) {	myDrawingView = newDrawingView;	}

		public DrawingView getDrawingView() { return myDrawingView;	}

		private void setPreviousDrawingView(DrawingView newPreviousDrawingView) {	myPreviousDrawingView = newPreviousDrawingView;	}

		public DrawingView getPreviousDrawingView() {	return myPreviousDrawingView;	}


}
