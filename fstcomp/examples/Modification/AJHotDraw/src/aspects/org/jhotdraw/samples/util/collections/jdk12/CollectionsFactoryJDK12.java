
package org.jhotdraw.util.collections.jdk12; 
import org.jhotdraw.util.CollectionsFactory; 
import java.util.*; 
public  class  CollectionsFactoryJDK12  extends CollectionsFactory {
		public CollectionsFactoryJDK12() {	}

		public List createList() {	return new ArrayList();	}

		public List createList(Collection initList) {	return new ArrayList(initList);	}

		public List createList(int initSize) {	return new ArrayList(initSize);	}

		public Map createMap() {	return new Hashtable();	}

		public Map createMap(Map initMap) {	return new Hashtable(initMap);	}

		public Set createSet() {	return new HashSet();	}

		public Set createSet(Set initSet) {	return new HashSet(initSet);	}


}
