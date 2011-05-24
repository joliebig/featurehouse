
package org.jhotdraw.standard; 
import org.jhotdraw.framework.Figure; 
 
class  OrderedFigureElement  implements Comparable {
		private Figure _theFigure;

		private int _nZ;

		public OrderedFigureElement(Figure aFigure, int nZ) {	_theFigure = aFigure;	_nZ = nZ;	}

		public Figure getFigure() {	return _theFigure;	}

		public int getZValue() {	return _nZ;	}

		public int compareTo(Object o) {	OrderedFigureElement ofe = (OrderedFigureElement) o;	if (_nZ == ofe.getZValue()) {	return 0;	}	if (_nZ > ofe.getZValue()) {	return 1;	}	return -1;	}


}
