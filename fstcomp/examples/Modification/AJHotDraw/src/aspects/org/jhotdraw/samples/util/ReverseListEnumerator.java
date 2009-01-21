
package org.jhotdraw.util; 
import java.util.List; 
import java.util.NoSuchElementException; 
import java.util.Iterator; 
public  class  ReverseListEnumerator  implements Iterator {
		private List myList;

		private int count;

		public ReverseListEnumerator(List l) {	myList = l;	count = myList.size() - 1;	}

		public boolean hasNext() {	return count >= 0;	}

		public Object next() {	if (count >= 0) {	return myList.get(count--);	}	throw new NoSuchElementException("ReverseListEnumerator");	}

		public void remove() {	myList.remove(count);	count--;	}


}
