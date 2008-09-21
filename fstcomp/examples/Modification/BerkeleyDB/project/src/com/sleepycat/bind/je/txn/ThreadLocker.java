package com.sleepycat.je.txn; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
public  class  ThreadLocker  extends BasicLocker {
	 public ThreadLocker( EnvironmentImpl env) throws DatabaseException { super(env); }

	 protected void checkState__wrappee__base( boolean ignoreCalledByAbort) throws DatabaseException { if (thread != Thread.currentThread()) { throw new DatabaseException("A per-thread transaction was" + " created in " + thread + " but used in "+ Thread.currentThread()); } }

	 protected void checkState( boolean ignoreCalledByAbort) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkState__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Locker newNonTxnLocker__wrappee__base() throws DatabaseException { checkState(false); return new ThreadLocker(envImpl); }

	 public Locker newNonTxnLocker() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	newNonTxnLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean sharesLocksWith__wrappee__base( Locker other){ if (super.sharesLocksWith(other)) { return true; } else if (other instanceof ThreadLocker) { return thread == ((ThreadLocker)other).thread; } else { return false; } }

	 public boolean sharesLocksWith( Locker other){ t.in(Thread.currentThread().getStackTrace()[1].toString());	sharesLocksWith__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
