package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  CursorConfig  implements Cloneable {
	 public final static CursorConfig DEFAULT=new CursorConfig();

	 public final static CursorConfig READ_UNCOMMITTED=new CursorConfig();

	 public final static CursorConfig DIRTY_READ=READ_UNCOMMITTED;

	 public final static CursorConfig READ_COMMITTED=new CursorConfig();

	
static { READ_UNCOMMITTED.setReadUncommitted(true); READ_COMMITTED.setReadCommitted(true); }

	 private boolean readUncommitted=false;

	 private boolean readCommitted=false;

	 public CursorConfig(){ }

	 public void setReadUncommitted__wrappee__base( boolean readUncommitted){ this.readUncommitted=readUncommitted; }

	 public void setReadUncommitted( boolean readUncommitted){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setReadUncommitted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getReadUncommitted__wrappee__base(){ return readUncommitted; }

	 public boolean getReadUncommitted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getReadUncommitted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setDirtyRead__wrappee__base( boolean dirtyRead){ setReadUncommitted(dirtyRead); }

	 public void setDirtyRead( boolean dirtyRead){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setDirtyRead__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getDirtyRead__wrappee__base(){ return getReadUncommitted(); }

	 public boolean getDirtyRead(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDirtyRead__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setReadCommitted__wrappee__base( boolean readCommitted){ this.readCommitted=readCommitted; }

	 public void setReadCommitted( boolean readCommitted){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setReadCommitted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getReadCommitted__wrappee__base(){ return readCommitted; }

	 public boolean getReadCommitted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getReadCommitted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 CursorConfig cloneConfig__wrappee__base(){ try { return (CursorConfig)super.clone(); } catch ( CloneNotSupportedException willNeverOccur) { return null; } }

	 CursorConfig cloneConfig(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	cloneConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
