
package org.jhotdraw.standard; 
import org.jhotdraw.framework.HandleEnumeration; 
import org.jhotdraw.framework.Handle; 
import java.util.List; 
public  class  HandleAndEnumerator  implements HandleEnumeration {
		private HandleEnumeration myHE1;

		private HandleEnumeration myHE2;

		public HandleAndEnumerator(HandleEnumeration newHE1, HandleEnumeration newHE2) {	myHE1 = newHE1;	myHE2 = newHE2;	}

		public Handle nextHandle() {	if (myHE1.hasNextHandle()) {	return myHE1.nextHandle();	}	else if (myHE2.hasNextHandle()) {	return myHE2.nextHandle();	}	else {	return null;	}	}

		public boolean hasNextHandle() {	return myHE1.hasNextHandle() || myHE2.hasNextHandle();	}

		public List toList() {	List joinedList = myHE1.toList();	joinedList.addAll(myHE2.toList());	return joinedList;	}

		public void reset() {	myHE1.reset();	myHE2.reset();	}


}
