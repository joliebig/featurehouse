
package org.jhotdraw.util.collections.jdk11; 
import org.jhotdraw.util.CollectionsFactory; 
import java.util.*; 
public  class  CollectionsFactoryJDK11  extends CollectionsFactory {
		public CollectionsFactoryJDK11() {	}

		public List createList() {	return new ListWrapper();	}

		public List createList(Collection initList) {	return new ListWrapper(initList);	}

		public List createList(int initSize) {	return new ListWrapper(initSize);	}

		public Map createMap() {	return new MapWrapper();	}

		public Map createMap(Map initMap) {	return new MapWrapper(initMap);	}

		public Set createSet() {	return new SetWrapper();	}

		public Set createSet(Set initSet) {	return new SetWrapper(initSet);	}


}
