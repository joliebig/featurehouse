
package org.jhotdraw.framework; 
import java.awt.Graphics; 
import java.awt.Point; 
import java.awt.Rectangle; 
import java.io.Serializable; 
public  interface  Connector  extends Serializable {
		public abstract Point findStart(ConnectionFigure connection);

		public abstract Point findEnd(ConnectionFigure connection);

		public abstract Figure owner();

		public abstract Rectangle displayBox();

		public abstract boolean containsPoint(int x, int y);

		public abstract void draw(Graphics g);

		public void connectorVisibility(boolean isVisible, ConnectionFigure courtingConnection);


}
