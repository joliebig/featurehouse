
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
public final  class  SingleFigureEnumerator  implements FigureEnumeration {
		private Figure mySingleFigure;

		private Figure myInitialFigure;

		public SingleFigureEnumerator(Figure newSingleFigure) {	myInitialFigure = newSingleFigure;	reset();	}

		public boolean hasNextFigure() {	return mySingleFigure != null;	}

		public Figure nextFigure() {	Figure returnFigure = mySingleFigure;	mySingleFigure = null;	return returnFigure;	}

		public void reset() {	mySingleFigure = myInitialFigure;	}


}
