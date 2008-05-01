package com.sleepycat.je.txn; 
import java.util.Set; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.LockStats; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.dbi.MemoryBudget; 
import de.ovgu.cide.jakutil.*; 
public  class  DummyLockManager  extends LockManager {
	 public DummyLockManager( EnvironmentImpl envImpl) throws DatabaseException { super(envImpl); }

	 protected LockAttemptResult attemptLock( Long nodeId, Locker locker, LockType type, boolean nonBlockingRequest) throws DatabaseException { return new LockAttemptResult(null,LockGrantType.NEW,true); }

	 protected String makeTimeoutMsg( String lockOrTxn, Locker locker, long nodeId, LockType type, LockGrantType grantType, Lock useLock, long timeout, long start, long now, DatabaseImpl database){ return null; }

	 protected Set releaseAndFindNotifyTargets( long nodeId, Lock lock, Locker locker, boolean removeFromLocker) throws DatabaseException { return null; }

	 void transfer( long nodeId, Locker owningLocker, Locker destLocker, boolean demoteToRead) throws DatabaseException { }

	 void transferMultiple( long nodeId, Locker owningLocker, Locker[] destLockers) throws DatabaseException { }

	 void demote( long nodeId, Locker locker) throws DatabaseException { }

	 boolean isLocked( Long nodeId){ return false; }

	 boolean isOwner( Long nodeId, Locker locker, LockType type){ return false; }

	 boolean isWaiter( Long nodeId, Locker locker){ return false; }

	 int nWaiters( Long nodeId){ return 0; }

	 int nOwners( Long nodeId){ return 0; }

	 Locker getWriteOwnerLocker( Long nodeId) throws DatabaseException { return null; }

	 protected boolean validateOwnership( Long nodeId, Locker locker, LockType type, boolean flushFromWaiters, MemoryBudget mb) throws DatabaseException { return true; }

	 protected void dumpLockTable( LockStats stats) throws DatabaseException { }


}
