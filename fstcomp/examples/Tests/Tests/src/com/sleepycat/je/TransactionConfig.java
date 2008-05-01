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

	 public void setSync( boolean sync){ this.sync=sync; }

	 public boolean getSync(){ return sync; }

	 public void setNoSync( boolean noSync){ this.noSync=noSync; }

	 public boolean getNoSync(){ return noSync; }

	 public void setWriteNoSync( boolean writeNoSync){ this.writeNoSync=writeNoSync; }

	 public boolean getWriteNoSync(){ return writeNoSync; }

	 public void setNoWait( boolean noWait){ this.noWait=noWait; }

	 public boolean getNoWait(){ return noWait; }

	 public void setReadUncommitted( boolean readUncommitted){ this.readUncommitted=readUncommitted; }

	 public boolean getReadUncommitted(){ return readUncommitted; }

	 public void setDirtyRead( boolean dirtyRead){ setReadUncommitted(dirtyRead); }

	 public boolean getDirtyRead(){ return getReadUncommitted(); }

	 public void setReadCommitted( boolean readCommitted){ this.readCommitted=readCommitted; }

	 public boolean getReadCommitted(){ return readCommitted; }

	 public void setSerializableIsolation( boolean serializableIsolation){ this.serializableIsolation=serializableIsolation; }

	 public boolean getSerializableIsolation(){ return serializableIsolation; }

	 TransactionConfig cloneConfig(){ try { return (TransactionConfig)super.clone(); } catch ( CloneNotSupportedException willNeverOccur) { return null; } }


}
