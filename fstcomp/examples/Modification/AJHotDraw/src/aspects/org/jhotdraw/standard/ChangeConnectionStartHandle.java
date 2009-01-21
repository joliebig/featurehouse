
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.Undoable; 
import java.awt.Point; 
public  class  ChangeConnectionStartHandle  extends ChangeConnectionHandle {
		public ChangeConnectionStartHandle(ConnectionFigure owner) {	super(owner);	}

		protected Connector target() {	return getConnection().getStartConnector();	}

		protected void disconnect() {	getConnection().disconnectStart();	}

		protected void connect(Connector c) {	getConnection().connectStart(c);	}

		protected void setPoint(int x, int y) {	getConnection().startPoint(x, y);	}

		public Point locate() {	return getConnection().startPoint();	}

		protected Undoable createUndoActivity(DrawingView newView) {	return new ChangeConnectionStartHandle.UndoActivity(newView);	}

		public static  class  UndoActivity  extends ChangeConnectionHandle.UndoActivity {
			public UndoActivity(DrawingView newView) {	super(newView);	}

			protected Connector replaceConnector(ConnectionFigure connection) {	Connector tempStartConnector = connection.getStartConnector();	connection.connectStart(getOldConnector());	return tempStartConnector;	}


	}

	 protected boolean canConnectTo(Figure figure) { return getConnection().canConnect(figure, source().owner());
}


}
