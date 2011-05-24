package com.sleepycat.je.txn; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
public  class  BuddyLocker  extends BasicLocker {
	 private Locker buddy;

	 public BuddyLocker( EnvironmentImpl env, Locker buddy) throws DatabaseException { super(env); this.buddy=buddy; }

	 Locker getBuddy__wrappee__base(){ return buddy; }

	 Locker getBuddy(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBuddy__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Txn getTxnLocker__wrappee__base(){ return buddy.getTxnLocker(); }

	 public Txn getTxnLocker(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Locker newNonTxnLocker__wrappee__base() throws DatabaseException { return new BuddyLocker(envImpl,buddy.newNonTxnLocker()); }

	 public Locker newNonTxnLocker() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	newNonTxnLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void releaseNonTxnLocks__wrappee__base() throws DatabaseException { super.releaseNonTxnLocks(); buddy.releaseNonTxnLocks(); }

	 public void releaseNonTxnLocks() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	releaseNonTxnLocks__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean sharesLocksWith__wrappee__base( Locker other){ if (super.sharesLocksWith(other)) { return true; } else { return buddy == other; } }

	 public boolean sharesLocksWith( Locker other){ t.in(Thread.currentThread().getStackTrace()[1].toString());	sharesLocksWith__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
