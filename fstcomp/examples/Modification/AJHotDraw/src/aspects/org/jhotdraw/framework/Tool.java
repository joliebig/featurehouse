
package org.jhotdraw.framework; 
import org.jhotdraw.util.Undoable; 
import java.awt.event.MouseEvent; 
import java.awt.event.KeyEvent; 
public  interface  Tool {
		public boolean isActive();

		public void activate();

		public void deactivate();

		public void mouseDown(MouseEvent e, int x, int y);

		public void mouseDrag(MouseEvent e, int x, int y);

		public void mouseUp(MouseEvent e, int x, int y);

		public void mouseMove(MouseEvent evt, int x, int y);

		public void keyDown(KeyEvent evt, int key);

		public boolean isEnabled();

		public void setEnabled(boolean enableUsableCheck);

		public boolean isUsable();

		public void setUsable(boolean newIsUsable);

		public DrawingEditor editor();

		public Undoable getUndoActivity();

		public void setUndoActivity(Undoable newUndoableActivity);

		public void addToolListener(ToolListener newToolListener);

		public void removeToolListener(ToolListener oldToolListener);


}
