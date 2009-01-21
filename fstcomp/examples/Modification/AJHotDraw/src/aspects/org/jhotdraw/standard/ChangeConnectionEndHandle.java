
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.Undoable; 
import java.awt.Point; 
public  class  ChangeConnectionEndHandle  extends ChangeConnectionHandle {
		public ChangeConnectionEndHandle(ConnectionFigure owner) {	super(owner);	}

		protected Connector target() {	return getConnection().getEndConnector();	}

		protected void disconnect() {	getConnection().disconnectEnd();	}

		protected void connect(Connector c) {	getConnection().connectEnd(c);	}

		protected void setPoint(int x, int y) {	getConnection().endPoint(x, y);	}

		public Point locate() {	return getConnection().endPoint();	}

		protected Undoable createUndoActivity(DrawingView newView) {	return new ChangeConnectionEndHandle.UndoActivity(newView);	}

		public static  class  UndoActivity  extends ChangeConnectionHandle.UndoActivity {
			public UndoActivity(DrawingView newView) {	super(newView);	}

			protected Connector replaceConnector(ConnectionFigure connection) {	Connector tempEndConnector = connection.getEndConnector();	connection.connectEnd(getOldConnector());	return tempEndConnector;	}


	}

	 protected boolean canConnectTo(Figure figure) { return getConnection().canConnect(source().owner(), figure);	}


}
