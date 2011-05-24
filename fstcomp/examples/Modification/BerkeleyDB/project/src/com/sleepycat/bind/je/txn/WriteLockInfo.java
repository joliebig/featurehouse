package com.sleepycat.je.txn; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  WriteLockInfo {
	 Lock lock;

	 long abortLsn=DbLsn.NULL_LSN;

	 boolean abortKnownDeleted;

	 boolean neverLocked;

	 boolean createdThisTxn;

	 static final WriteLockInfo basicWriteLockInfo=new WriteLockInfo();

	 WriteLockInfo( Lock lock){ this.lock=lock; abortLsn=DbLsn.NULL_LSN; abortKnownDeleted=false; neverLocked=true; createdThisTxn=false; }

	 WriteLockInfo(){ this.lock=null; abortLsn=DbLsn.NULL_LSN; abortKnownDeleted=true; neverLocked=true; createdThisTxn=false; }

	 public boolean getAbortKnownDeleted__wrappee__base(){ return abortKnownDeleted; }

	 public boolean getAbortKnownDeleted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getAbortKnownDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getAbortLsn__wrappee__base(){ return abortLsn; }

	 public long getAbortLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getAbortLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
