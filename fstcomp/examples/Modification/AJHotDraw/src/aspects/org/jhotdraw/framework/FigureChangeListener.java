
package org.jhotdraw.framework; 
import java.util.EventListener; 
public  interface  FigureChangeListener  extends EventListener {
		public void figureInvalidated(FigureChangeEvent e);

		public void figureChanged(FigureChangeEvent e);

		public void figureRemoved(FigureChangeEvent e);

		public void figureRequestRemove(FigureChangeEvent e);

		public void figureRequestUpdate(FigureChangeEvent e);


}
