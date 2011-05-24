
package org.jhotdraw.standard; 
import org.jhotdraw.framework.FigureEnumeration; 
import org.jhotdraw.framework.Figure; 
public  class  FigureAndEnumerator  implements FigureEnumeration {
		private FigureEnumeration myFE1;

		private FigureEnumeration myFE2;

		public FigureAndEnumerator(FigureEnumeration newFE1, FigureEnumeration newFE2) {	myFE1 = newFE1;	myFE2 = newFE2;	}

		public Figure nextFigure() {	if (myFE1.hasNextFigure()) {	return myFE1.nextFigure();	}	else if (myFE2.hasNextFigure()) {	return myFE2.nextFigure();	}	else {	return null;	}	}

		public boolean hasNextFigure() {	return myFE1.hasNextFigure() || myFE2.hasNextFigure();	}

		public void reset() {	myFE1.reset();	myFE2.reset();	}


}
