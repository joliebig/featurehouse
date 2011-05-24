
package org.jhotdraw.framework; 
public  interface  DrawingChangeListener {
		public void drawingInvalidated(DrawingChangeEvent e);

	 public void drawingTitleChanged(DrawingChangeEvent e);

		public void drawingRequestUpdate(DrawingChangeEvent e);


}
