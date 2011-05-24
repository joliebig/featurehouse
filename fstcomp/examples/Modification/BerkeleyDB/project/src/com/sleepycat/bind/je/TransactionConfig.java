package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  TransactionConfig  implements Cloneable {
	 public static final TransactionConfig DEFAULT=new TransactionConfig();

	 private boolean sync=false;

	 private boolean noSync=false;

	 private boolean writeNoSync=false;

	 private boolean noWait=false;

	 private boolean readUncommitted=false;

	 private boolean readCommitted=false;

	 private boolean serializableIsolation=false;

	 public TransactionConfig(){ }

	 public void setSync__wrappee__base( boolean sync){ this.sync=sync; }

	 public void setSync( boolean sync){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getSync__wrappee__base(){ return sync; }

	 public boolean getSync(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setNoSync__wrappee__base( boolean noSync){ this.noSync=noSync; }

	 public void setNoSync( boolean noSync){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setNoSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getNoSync__wrappee__base(){ return noSync; }

	 public boolean getNoSync(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNoSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setWriteNoSync__wrappee__base( boolean writeNoSync){ this.writeNoSync=writeNoSync; }

	 public void setWriteNoSync( boolean writeNoSync){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setWriteNoSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getWriteNoSync__wrappee__base(){ return writeNoSync; }

	 public boolean getWriteNoSync(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getWriteNoSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setNoWait__wrappee__base( boolean noWait){ this.noWait=noWait; }

	 public void setNoWait( boolean noWait){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setNoWait__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getNoWait__wrappee__base(){ return noWait; }

	 public boolean getNoWait(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNoWait__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

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

	 public void setSerializableIsolation__wrappee__base( boolean serializableIsolation){ this.serializableIsolation=serializableIsolation; }

	 public void setSerializableIsolation( boolean serializableIsolation){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setSerializableIsolation__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getSerializableIsolation__wrappee__base(){ return serializableIsolation; }

	 public boolean getSerializableIsolation(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getSerializableIsolation__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 TransactionConfig cloneConfig__wrappee__base(){ try { return (TransactionConfig)super.clone(); } catch ( CloneNotSupportedException willNeverOccur) { return null; } }

	 TransactionConfig cloneConfig(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	cloneConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
