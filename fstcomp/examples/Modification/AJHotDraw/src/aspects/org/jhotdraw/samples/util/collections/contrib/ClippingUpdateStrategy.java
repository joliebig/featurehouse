package org.jhotdraw.contrib; 
import java.awt.Graphics; 
import java.awt.Rectangle; 
import java.util.Vector; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.FigureEnumerator; 
public  class  ClippingUpdateStrategy  implements Painter {
		public ClippingUpdateStrategy() {	super();	}

		public void draw(Graphics g, DrawingView view) {	Rectangle viewClipRectangle = g.getClipBounds();	if (viewClipRectangle == null) {	view.drawAll(g);	return;	}	FigureEnumeration fe = view.drawing().figures();	Vector v = new Vector(1000);	while (fe.hasNextFigure()) {	Figure fig = fe.nextFigure();	Rectangle r = fig.displayBox();	if (r.width <= 0) {	r.width = 1;	}	if (r.height <= 0) {	r.height = 1;	}	if (r.intersects(viewClipRectangle)) {	v.add(fig);	}	}	FigureEnumeration clippedFE = new FigureEnumerator(v);	view.draw(g, clippedFE);	}


}
