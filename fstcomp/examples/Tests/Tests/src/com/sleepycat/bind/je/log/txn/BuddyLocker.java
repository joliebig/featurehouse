package com.sleepycat.je.txn; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
public  class  BuddyLocker  extends BasicLocker {
	 private Locker buddy;

	 public BuddyLocker( EnvironmentImpl env, Locker buddy) throws DatabaseException { super(env); this.buddy=buddy; }

	 Locker getBuddy(){ return buddy; }

	 public Txn getTxnLocker(){ return buddy.getTxnLocker(); }

	 public Locker newNonTxnLocker() throws DatabaseException { return new BuddyLocker(envImpl,buddy.newNonTxnLocker()); }

	 public void releaseNonTxnLocks() throws DatabaseException { super.releaseNonTxnLocks(); buddy.releaseNonTxnLocks(); }

	 public boolean sharesLocksWith( Locker other){ if (super.sharesLocksWith(other)) { return true; } else { return buddy == other; } }


}
