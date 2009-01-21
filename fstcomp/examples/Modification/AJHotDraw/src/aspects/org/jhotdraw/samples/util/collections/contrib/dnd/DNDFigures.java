
package org.jhotdraw.contrib.dnd; 
import org.jhotdraw.framework.FigureEnumeration; 
import org.jhotdraw.standard.FigureEnumerator; 
import org.jhotdraw.util.CollectionsFactory; 
import java.awt.Point; 
import java.util.List; 
public  class  DNDFigures  implements java.io.Serializable {
		private List figures;

		private Point origin;

		public DNDFigures(FigureEnumeration fe, Point newOrigin) {	this.figures = CollectionsFactory.current().createList();	while (fe.hasNextFigure()) {	figures.add(fe.nextFigure());	}	origin = newOrigin;	}

		public FigureEnumeration getFigures() { return new FigureEnumerator(figures);	}

		public Point getOrigin() { return origin;	}


}
