package org.jhotdraw.framework; 
import java.util.EventListener; 
public  interface  ViewChangeListener  extends EventListener {
		public void viewSelectionChanged(DrawingView oldView, DrawingView newView);

		public void viewCreated(DrawingView view);

		public void viewDestroying(DrawingView view);


}
