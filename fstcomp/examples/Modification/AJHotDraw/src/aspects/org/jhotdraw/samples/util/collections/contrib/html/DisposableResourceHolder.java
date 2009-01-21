
package org.jhotdraw.contrib.html; 
public  interface  DisposableResourceHolder {
		public Object getResource() throws NullPointerException;

		public Object clone();

		public void setResource(Object resource);

		public void setDisposableDelay(long millis);

		public long getDisposableDelay();

		public void resetDelay();

		public long getLastTimeAccessed();

		public void dispose();

		public boolean isAvailable();

		public void lock();

		public void unlock();

		public boolean isLocked();


}
