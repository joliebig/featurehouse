package com.sleepycat.je.txn; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
public  class  ThreadLocker  extends BasicLocker {
	 public ThreadLocker( EnvironmentImpl env) throws DatabaseException { super(env); }

	 protected void checkState( boolean ignoreCalledByAbort) throws DatabaseException { if (thread != Thread.currentThread()) { throw new DatabaseException("A per-thread transaction was" + " created in " + thread + " but used in "+ Thread.currentThread()); } }

	 public Locker newNonTxnLocker() throws DatabaseException { checkState(false); return new ThreadLocker(envImpl); }

	 public boolean sharesLocksWith( Locker other){ if (super.sharesLocksWith(other)) { return true; } else if (other instanceof ThreadLocker) { return thread == ((ThreadLocker)other).thread; } else { return false; } }


}
