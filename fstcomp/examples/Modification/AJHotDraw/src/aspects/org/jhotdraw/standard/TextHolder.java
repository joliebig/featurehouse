
package org.jhotdraw.standard; 
import java.awt.*; 
import org.jhotdraw.framework.*; 
public  interface  TextHolder {
		public Rectangle textDisplayBox();

		public String getText();

		public void setText(String newText);

		public boolean acceptsTyping();

		public int overlayColumns();

		public void connect(Figure connectedFigure);

		public void disconnect(Figure disconnectFigure);

		public Font getFont();

		public Figure getRepresentingFigure();


}
