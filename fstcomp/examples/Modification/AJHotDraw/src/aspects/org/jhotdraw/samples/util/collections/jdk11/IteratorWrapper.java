
package org.jhotdraw.util.collections.jdk11; 
import java.util.Iterator; 
import java.util.Enumeration; 
public  class  IteratorWrapper  implements Iterator {
		private Enumeration myEnumeration;

		public IteratorWrapper(Enumeration enumeration) {	myEnumeration = enumeration;	}

		public boolean hasNext() {	return myEnumeration.hasMoreElements();	}

		public Object next() {	return myEnumeration.nextElement();	}

		public void remove() {	}


}
