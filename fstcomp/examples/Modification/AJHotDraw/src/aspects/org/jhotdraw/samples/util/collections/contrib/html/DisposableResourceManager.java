
package org.jhotdraw.contrib.html; 
import java.util.Iterator; 
public  interface  DisposableResourceManager {
		public void registerResource(DisposableResourceHolder resource);

		public void unregisterResource(DisposableResourceHolder resource);

		public Iterator getResources();

		public boolean managesResource(DisposableResourceHolder resource);

		public void startDisposing() throws ResourceManagerNotSetException;

		public void stopDisposing(long millis);


}
