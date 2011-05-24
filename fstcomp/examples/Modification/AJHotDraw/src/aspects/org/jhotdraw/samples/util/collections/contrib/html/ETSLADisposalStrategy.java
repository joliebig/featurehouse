
package org.jhotdraw.contrib.html; 
import java.util.Iterator; 
public  class  ETSLADisposalStrategy  implements ResourceDisposabilityStrategy {
		private DisposableResourceManager manager;

		private long gcPeriodicity = 60000;

		private DisposalThread disposalThread = null;

		private boolean disposingActive = false;

		public ETSLADisposalStrategy() { }

		public ETSLADisposalStrategy(long periodicity) {	this(null, periodicity);	}

		public ETSLADisposalStrategy(DisposableResourceManager newManager, long newPeriodicity) {	setManager(newManager);	setPeriodicity(newPeriodicity);	initDisposalThread();	}

		public synchronized void setManager(DisposableResourceManager newManager) {	if (getManager() == null) {	stopDisposing(Long.MAX_VALUE);	}	manager = newManager;	}

		public DisposableResourceManager getManager() {	return manager;	}

		public void startDisposing() throws ResourceManagerNotSetException {	if (getManager() == null) {	throw new ResourceManagerNotSetException();	}	if (disposingActive) {	return;	}	disposingActive = true;	disposalThread.start();	}

		public void stopDisposing(long millis) {	if (!disposingActive) {	return;	}	try {	disposalThread.interruptDisposalPending = true;	disposalThread.join(millis);	}	catch (InterruptedException ex) {	}	finally {	disposingActive = false;	}	}

		protected void initDisposalThread() {	if (disposalThread != null) {	return;	}	disposalThread = new DisposalThread(this, getPeriodicity());	}

		protected synchronized void dispose() {	synchronized (getManager()) {	long currentTime = System.currentTimeMillis();	Iterator resourceIter = getManager().getResources();	DisposableResourceHolder resource;	while (resourceIter.hasNext()) {	resource = (DisposableResourceHolder)resourceIter.next();	synchronized (resource) {	if (!resource.isLocked() && (resource.getLastTimeAccessed() + resource.getDisposableDelay()) < currentTime) {	resource.dispose();	}	}	}	}	}

		public long getPeriodicity() {	return gcPeriodicity;	}

		public void setPeriodicity(long newPeriodicity) {	gcPeriodicity = newPeriodicity;	if (disposalThread != null) {	disposalThread.setPeriodicity(newPeriodicity);	}	}


} 
 
class  DisposalThread  extends Thread {
		private ETSLADisposalStrategy strategy;

		private long periodicity = 60000;

		boolean interruptDisposalPending = false;

		DisposalThread(ETSLADisposalStrategy newStrategy, long newPeriodicity) {	strategy = newStrategy;	periodicity = newPeriodicity;	}

		public void run() {	interruptDisposalPending = false;	while (!interruptDisposalPending) {	try {	sleep(periodicity);	}	catch (Exception ex) {	break;	}	strategy.dispose();	}	interruptDisposalPending = false;	}

		public long getPeriodicity() {	return periodicity;	}

		public void setPeriodicity(long newPeriodicity) {	periodicity = newPeriodicity;	}

		public void interruptDisposal() {	interruptDisposalPending = true;	}


}
