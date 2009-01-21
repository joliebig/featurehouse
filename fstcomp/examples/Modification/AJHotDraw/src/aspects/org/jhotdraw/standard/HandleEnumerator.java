
package org.jhotdraw.standard; 
import org.jhotdraw.util.CollectionsFactory; 
import org.jhotdraw.framework.HandleEnumeration; 
import org.jhotdraw.framework.Handle; 
import java.util.Iterator; 
import java.util.Collection; 
import java.util.List; 
public  class  HandleEnumerator  implements HandleEnumeration {
		private Iterator myIterator;

		private Collection myInitialCollection;

		private static HandleEnumerator singletonEmptyEnumerator =	new HandleEnumerator(CollectionsFactory.current().createList());

		public HandleEnumerator(Collection c) {	myInitialCollection = c;	reset();	}

		public boolean hasNextHandle() {	return myIterator.hasNext();	}

		public Handle nextHandle() {	return (Handle)myIterator.next();	}

		public List toList() {	List handles = CollectionsFactory.current().createList();	while (hasNextHandle()) {	handles.add(nextHandle());	}	myIterator = handles.iterator();	return handles;	}

		public void reset() {	myIterator = myInitialCollection.iterator();	}

		public static HandleEnumeration getEmptyEnumeration() {	return singletonEmptyEnumerator;	}


}
