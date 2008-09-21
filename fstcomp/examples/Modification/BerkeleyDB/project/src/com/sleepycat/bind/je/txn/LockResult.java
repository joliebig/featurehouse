package com.sleepycat.je.txn; 
import com.sleepycat.je.tree.LN; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  LockResult {
	 private LockGrantType grant;

	 private WriteLockInfo info;

	 private LN ln;

	 public LockResult( LockGrantType grant, WriteLockInfo info){ this.grant=grant; this.info=info; }

	 public LN getLN__wrappee__base(){ return ln; }

	 public LN getLN(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setLN__wrappee__base( LN ln){ this.ln=ln; }

	 public void setLN( LN ln){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LockGrantType getLockGrant__wrappee__base(){ return grant; }

	 public LockGrantType getLockGrant(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLockGrant__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setAbortLsn__wrappee__base( long abortLsn, boolean abortKnownDeleted){ setAbortLsnInternal(abortLsn,abortKnownDeleted,false); }

	 public void setAbortLsn( long abortLsn, boolean abortKnownDeleted){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setAbortLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setAbortLsn__wrappee__base( long abortLsn, boolean abortKnownDeleted, boolean createdThisTxn){ setAbortLsnInternal(abortLsn,abortKnownDeleted,createdThisTxn); }

	 public void setAbortLsn( long abortLsn, boolean abortKnownDeleted, boolean createdThisTxn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setAbortLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void setAbortLsnInternal__wrappee__base( long abortLsn, boolean abortKnownDeleted, boolean createdThisTxn){ if (info != null && info.neverLocked) { if (abortLsn != DbLsn.NULL_LSN) { info.abortLsn=abortLsn; info.abortKnownDeleted=abortKnownDeleted; } info.createdThisTxn=createdThisTxn; info.neverLocked=false; } }

	 private void setAbortLsnInternal( long abortLsn, boolean abortKnownDeleted, boolean createdThisTxn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setAbortLsnInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
