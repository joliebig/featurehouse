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

	 public void setReadUncommitted( boolean readUncommitted){ this.readUncommitted=readUncommitted; }

	 public boolean getReadUncommitted(){ return readUncommitted; }

	 public void setDirtyRead( boolean dirtyRead){ setReadUncommitted(dirtyRead); }

	 public boolean getDirtyRead(){ return getReadUncommitted(); }

	 public void setReadCommitted( boolean readCommitted){ this.readCommitted=readCommitted; }

	 public boolean getReadCommitted(){ return readCommitted; }

	 CursorConfig cloneConfig(){ try { return (CursorConfig)super.clone(); } catch ( CloneNotSupportedException willNeverOccur) { return null; } }


}
