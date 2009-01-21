
package org.jhotdraw.standard; 
import org.jhotdraw.util.ReverseListEnumerator; 
import org.jhotdraw.framework.*; 
import java.util.Iterator; 
import java.util.List; 
public final  class  ReverseFigureEnumerator  implements FigureEnumeration {
		private Iterator myIterator;

		private List myInitialList;

		public ReverseFigureEnumerator(List l) {	myInitialList = l;	reset();	}

		public boolean hasNextFigure() {	return myIterator.hasNext();	}

		public Figure nextFigure() {	return (Figure)myIterator.next();	}

		public void reset() {	myIterator = new ReverseListEnumerator(myInitialList);	}


}
