
package org.jhotdraw.util.collections.jdk11; 
import java.util.*; 
public  class  MapWrapper  implements Map {
		private Map myDelegee;

		public MapWrapper() {	myDelegee = new Hashtable();	}

		public MapWrapper(Map copyMap) {	myDelegee = new Hashtable(copyMap);	}

		public int size() {	return myDelegee.size();	}

		public boolean isEmpty() {	return myDelegee.isEmpty();	}

		public boolean containsKey(Object key) {	return myDelegee.containsKey(key);	}

		public boolean containsValue(Object value) {	return myDelegee.containsKey(value);	}

		public Object get(Object key) {	return myDelegee.get(key);	}

		public Object put(Object key, Object value) {	return myDelegee.put(key, value);	}

		public Object remove(Object key) {	return myDelegee.remove(key);	}

		public void putAll(Map t) {	myDelegee.putAll(t);	}

		public void clear() {	myDelegee.clear();	}

		public Set keySet() {	return myDelegee.keySet();	}

		public Collection values() {	return myDelegee.values();	}

		public Set entrySet() {	return myDelegee.entrySet();	}


}
