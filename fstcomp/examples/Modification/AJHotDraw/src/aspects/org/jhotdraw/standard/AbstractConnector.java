
package org.jhotdraw.standard; 
import java.awt.Graphics; 
import java.awt.Point; 
import java.awt.Rectangle; 
import org.jhotdraw.framework.ConnectionFigure; 
import org.jhotdraw.framework.Connector; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.util.Geom; 
public abstract  class  AbstractConnector  implements Connector {
		private Figure fOwner;

		private static final long serialVersionUID = -5170007865562687545L;

		private int abstractConnectorSerializedDataVersion = 1;

		public AbstractConnector() {	fOwner = null;	}

		public AbstractConnector(Figure owner) {	fOwner = owner;	}

		public Figure owner() {	return fOwner;	}

		public Point findStart(ConnectionFigure connection) {	return findPoint(connection);	}

		public Point findEnd(ConnectionFigure connection) {	return findPoint(connection);	}

		protected Point findPoint(ConnectionFigure connection) {	return Geom.center(displayBox());	}

		public Rectangle displayBox() {	return owner().displayBox();	}

		public boolean containsPoint(int x, int y) {	return owner().containsPoint(x, y);	}

		public void draw(Graphics g) {	}

		public void connectorVisibility(boolean isVisible, ConnectionFigure courtingConnection) {	}


}
