
package org.jhotdraw.framework; 
import java.awt.Rectangle; 
import java.util.EventObject; 
public  class  DrawingChangeEvent  extends EventObject {
		private Rectangle myRectangle;

		public DrawingChangeEvent(Drawing newSource, Rectangle newRect) {	super(newSource);	myRectangle = newRect;	}

		public Drawing getDrawing() {	return (Drawing)getSource();	}

		public Rectangle getInvalidatedRectangle() {	return myRectangle;	}


}
