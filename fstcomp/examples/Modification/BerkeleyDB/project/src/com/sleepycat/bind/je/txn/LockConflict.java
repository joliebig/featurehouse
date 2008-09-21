package com.sleepycat.je.txn; 
import de.ovgu.cide.jakutil.*; 
 
class  LockConflict {
	 static final LockConflict ALLOW=new LockConflict(true,false);

	 static final LockConflict BLOCK=new LockConflict(false,false);

	 static final LockConflict RESTART=new LockConflict(false,true);

	 private boolean allowed;

	 private boolean restart;

	 private LockConflict( boolean allowed, boolean restart){ this.allowed=allowed; this.restart=restart; }

	 boolean getAllowed__wrappee__base(){ return allowed; }

	 boolean getAllowed(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getAllowed__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean getRestart__wrappee__base(){ return restart; }

	 boolean getRestart(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getRestart__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
