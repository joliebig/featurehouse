
package org.jhotdraw.contrib.html; 
public  interface  ResourceDisposabilityStrategy {
		public void setManager(DisposableResourceManager manager);

		public DisposableResourceManager getManager();

		public void startDisposing() throws ResourceManagerNotSetException;

		public void stopDisposing(long millis);


}
