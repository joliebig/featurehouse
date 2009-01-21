
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.CollectionsFactory; 
import java.util.Iterator; 
import java.util.Collection; 
public final  class  FigureEnumerator  implements FigureEnumeration {
		private Iterator myIterator;

		private Collection myInitialCollection;

		private static FigureEnumerator singletonEmptyEnumerator =	new FigureEnumerator(CollectionsFactory.current().createList());

		public FigureEnumerator(Collection c) {	myInitialCollection = c;	reset();	}

		public boolean hasNextFigure() {	return myIterator.hasNext();	}

		public Figure nextFigure() {	return (Figure)myIterator.next();	}

		public static FigureEnumeration getEmptyEnumeration() {	return singletonEmptyEnumerator;	}

		public void reset() {	myIterator = myInitialCollection.iterator();	}


}
