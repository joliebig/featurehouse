
package org.jhotdraw.contrib.dnd; 
import java.awt.dnd.DragGestureListener; 
import java.awt.dnd.DragSourceListener; 
public  interface  DNDInterface {
		public void DNDInitialize(DragGestureListener dgl);

		public void DNDDeinitialize();

		public DragSourceListener getDragSourceListener();


}
