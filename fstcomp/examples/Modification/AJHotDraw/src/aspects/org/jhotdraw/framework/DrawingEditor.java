
package org.jhotdraw.framework; 
import org.jhotdraw.util.UndoManager; 
public  interface  DrawingEditor {
		public DrawingView view();

		public DrawingView[] views();

		public Tool tool();

		public void toolDone();

		public void addViewChangeListener(ViewChangeListener vsl);

		public void removeViewChangeListener(ViewChangeListener vsl);

		public void showStatus(String string);

		public UndoManager getUndoManager();


}
