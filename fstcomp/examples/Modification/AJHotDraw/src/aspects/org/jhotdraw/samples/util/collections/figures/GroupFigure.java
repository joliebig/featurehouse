
package org.jhotdraw.figures; 
import java.awt.*; 
import java.util.List; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.util.CollectionsFactory; 
public  class  GroupFigure  extends CompositeFigure {
		private static final long serialVersionUID = 8311226373023297933L;

		private int groupFigureSerializedDataVersion = 1;

		public boolean canConnect() {	return false;	}

		public Rectangle displayBox() {	FigureEnumeration fe = figures();	Rectangle r = fe.nextFigure().displayBox();	while (fe.hasNextFigure()) {	r.add(fe.nextFigure().displayBox());	}	return r;	}

		public void basicDisplayBox(Point origin, Point corner) {	}

		public FigureEnumeration decompose() {	return new FigureEnumerator(fFigures);	}

		public HandleEnumeration handles() {	List handles = CollectionsFactory.current().createList();	handles.add(new GroupHandle(this, RelativeLocator.northWest()));	handles.add(new GroupHandle(this, RelativeLocator.northEast()));	handles.add(new GroupHandle(this, RelativeLocator.southWest()));	handles.add(new GroupHandle(this, RelativeLocator.southEast()));	return new HandleEnumerator(handles);	}

		public void setAttribute(String name, Object value) {	super.setAttribute(name, value);	FigureEnumeration fe = figures();	while (fe.hasNextFigure()) {	fe.nextFigure().setAttribute(name, value);	}	}

		public void setAttribute(FigureAttributeConstant fac, Object object){	super.setAttribute(fac, object);	FigureEnumeration fe = figures();	while (fe.hasNextFigure()) {	fe.nextFigure().setAttribute(fac, object);	}	}


}
