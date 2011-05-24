
package org.jhotdraw.contrib.html; 
public abstract  class  DisposableResourceManagerFactory {
		public static long DEFAULT_DISPOSAL_PERIODICITY = 60000;

		protected static DisposableResourceManager currentManager = null;

		protected static ResourceDisposabilityStrategy currentStrategy = null;

		protected static DisposableResourceHolder holderPrototype = null;

		public static DisposableResourceManager getManager() {	return currentManager;	}

		public static void setStrategy(ResourceDisposabilityStrategy strategy) {	currentStrategy = strategy;	}

		public static DisposableResourceHolder createStandardHolder(Object resource) {	initManager();	DisposableResourceHolder holder = (DisposableResourceHolder)holderPrototype.clone();	holder.setResource(resource);	getManager().registerResource(holder);	return holder;	}

		protected static void initManager() {	if (currentManager == null) {	if (holderPrototype == null) {	holderPrototype = new StandardDisposableResourceHolder();	}	if (currentStrategy == null) {	currentStrategy = new ETSLADisposalStrategy(DEFAULT_DISPOSAL_PERIODICITY);	}	if (currentManager == null) { currentManager = new StandardDisposableResourceManager(currentStrategy);	}	try {	currentManager.startDisposing();	}	catch (ResourceManagerNotSetException ex) {	}	}	}


}
