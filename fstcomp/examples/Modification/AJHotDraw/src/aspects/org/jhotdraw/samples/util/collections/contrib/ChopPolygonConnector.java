
package org.jhotdraw.contrib; 
import java.awt.*; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
public  class  ChopPolygonConnector  extends ChopBoxConnector {
		private static final long serialVersionUID = -156024908227796826L;

		public ChopPolygonConnector() {	}

		public ChopPolygonConnector(Figure owner) {	super(owner);	}

		protected Point chop(Figure target, Point from) {	return ((PolygonFigure)target).chop(from);	}


}
