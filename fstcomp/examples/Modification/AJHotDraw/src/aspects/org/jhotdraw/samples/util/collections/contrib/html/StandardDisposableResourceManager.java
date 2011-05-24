
package org.jhotdraw.contrib.html; 
import java.util.Iterator; 
import java.util.WeakHashMap; 
public  class  StandardDisposableResourceManager  implements DisposableResourceManager {
		private WeakHashMap resources;

		private ResourceDisposabilityStrategy strategy;

		public StandardDisposableResourceManager(ResourceDisposabilityStrategy newStrategy) {	resources = new WeakHashMap();	setStrategy(newStrategy);	getStrategy().setManager(this);	}

		public synchronized void registerResource(DisposableResourceHolder resource) {	resources.put(resource, resource);	}

		public synchronized void unregisterResource(DisposableResourceHolder resource) {	resources.remove(resource);	}

		public Iterator getResources() {	return resources.values().iterator();	}

		public synchronized boolean managesResource(DisposableResourceHolder resource) {	return resources.containsValue(resource);	}

		public ResourceDisposabilityStrategy getStrategy() {	return strategy;	}

		public void setStrategy(ResourceDisposabilityStrategy newStrategy) {	strategy = newStrategy;	}

		public void startDisposing() throws ResourceManagerNotSetException {	getStrategy().startDisposing();	}

		public void stopDisposing(long millis) {	getStrategy().stopDisposing(millis);	}


}
