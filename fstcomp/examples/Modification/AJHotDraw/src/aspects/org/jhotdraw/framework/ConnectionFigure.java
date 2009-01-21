
package org.jhotdraw.framework; 
import java.awt.Point; 
public  interface  ConnectionFigure  extends Figure, FigureChangeListener {
		public void connectStart(Connector start);

		public void connectEnd(Connector end);

		public void updateConnection();

		public void disconnectStart();

		public void disconnectEnd();

		public Connector getStartConnector();

		public Connector getEndConnector();

		public boolean canConnect(Figure start, Figure end);

		public boolean connectsSame(ConnectionFigure other);

		public void startPoint(int x, int y);

		public void endPoint(int x, int y);

		public Point startPoint();

		public Point endPoint();

		public void setPointAt(Point p, int index);

		public Point pointAt(int index);

		public int pointCount();

		public int splitSegment(int x, int y);

		public boolean joinSegments(int x, int y);

		public Figure startFigure();

		public Figure endFigure();


}
