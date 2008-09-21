package com.sleepycat.je.log; 
import de.ovgu.cide.jakutil.*; 
 
class  LogResult {
	 long currentLsn;

	 boolean wakeupCleaner;

	 LogResult( long currentLsn, boolean wakeupCheckpointer, boolean wakeupCleaner){ this.currentLsn=currentLsn; this.hook510(wakeupCheckpointer); this.wakeupCleaner=wakeupCleaner; }

	 protected void hook510__wrappee__base( boolean wakeupCheckpointer){ }

	 protected void hook510( boolean wakeupCheckpointer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook510__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
