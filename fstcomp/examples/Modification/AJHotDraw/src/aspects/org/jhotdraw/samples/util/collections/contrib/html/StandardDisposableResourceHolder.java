
package org.jhotdraw.contrib.html; 
import java.io.Serializable; 
public  class  StandardDisposableResourceHolder  implements DisposableResourceHolder, Serializable {
		private Object resource = null;

		private long disposeDelay = 60000;

		private long lastTimeAccessed = 0;

		private boolean isLocked = false;

		public StandardDisposableResourceHolder() { }

		public StandardDisposableResourceHolder(Object newResource) {	resource = newResource;	resetDelay();	}

		public Object clone() {	StandardDisposableResourceHolder clone = new StandardDisposableResourceHolder();	clone.setDisposableDelay(this.getDisposableDelay());	return clone;	}

		public Object getResource() throws NullPointerException {	if (resource != null) {	resetDelay();	return resource;	}	throw new NullPointerException();	}

		public void setResource(Object newResource) {	resource = newResource;	resetDelay();	}

		public void setDisposableDelay(long millis) {	disposeDelay = millis;	}

		public long getDisposableDelay() {	return disposeDelay;	}

		public void dispose() {	resource = null;	}

		public boolean isAvailable() {	return (resource != null);	}

		public void lock() {	isLocked = true;	}

		public void unlock() {	resetDelay();	isLocked = false;	}

		public boolean isLocked() {	return isLocked;	}

		public long getLastTimeAccessed() {	return lastTimeAccessed;	}

		public void resetDelay() {	lastTimeAccessed = System.currentTimeMillis();	}


}
