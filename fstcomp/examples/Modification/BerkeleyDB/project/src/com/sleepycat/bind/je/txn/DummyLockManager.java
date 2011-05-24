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

	 protected LockAttemptResult attemptLock__wrappee__base( Long nodeId, Locker locker, LockType type, boolean nonBlockingRequest) throws DatabaseException { return new LockAttemptResult(null,LockGrantType.NEW,true); }

	 protected LockAttemptResult attemptLock( Long nodeId, Locker locker, LockType type, boolean nonBlockingRequest) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	attemptLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected String makeTimeoutMsg__wrappee__base( String lockOrTxn, Locker locker, long nodeId, LockType type, LockGrantType grantType, Lock useLock, long timeout, long start, long now, DatabaseImpl database){ return null; }

	 protected String makeTimeoutMsg( String lockOrTxn, Locker locker, long nodeId, LockType type, LockGrantType grantType, Lock useLock, long timeout, long start, long now, DatabaseImpl database){ t.in(Thread.currentThread().getStackTrace()[1].toString());	makeTimeoutMsg__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected Set releaseAndFindNotifyTargets__wrappee__base( long nodeId, Lock lock, Locker locker, boolean removeFromLocker) throws DatabaseException { return null; }

	 protected Set releaseAndFindNotifyTargets( long nodeId, Lock lock, Locker locker, boolean removeFromLocker) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	releaseAndFindNotifyTargets__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void transfer__wrappee__base( long nodeId, Locker owningLocker, Locker destLocker, boolean demoteToRead) throws DatabaseException { }

	 void transfer( long nodeId, Locker owningLocker, Locker destLocker, boolean demoteToRead) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	transfer__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void transferMultiple__wrappee__base( long nodeId, Locker owningLocker, Locker[] destLockers) throws DatabaseException { }

	 void transferMultiple( long nodeId, Locker owningLocker, Locker[] destLockers) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	transferMultiple__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void demote__wrappee__base( long nodeId, Locker locker) throws DatabaseException { }

	 void demote( long nodeId, Locker locker) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	demote__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isLocked__wrappee__base( Long nodeId){ return false; }

	 boolean isLocked( Long nodeId){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isLocked__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isOwner__wrappee__base( Long nodeId, Locker locker, LockType type){ return false; }

	 boolean isOwner( Long nodeId, Locker locker, LockType type){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isOwner__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isWaiter__wrappee__base( Long nodeId, Locker locker){ return false; }

	 boolean isWaiter( Long nodeId, Locker locker){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isWaiter__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 int nWaiters__wrappee__base( Long nodeId){ return 0; }

	 int nWaiters( Long nodeId){ t.in(Thread.currentThread().getStackTrace()[1].toString());	nWaiters__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 int nOwners__wrappee__base( Long nodeId){ return 0; }

	 int nOwners( Long nodeId){ t.in(Thread.currentThread().getStackTrace()[1].toString());	nOwners__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 Locker getWriteOwnerLocker__wrappee__base( Long nodeId) throws DatabaseException { return null; }

	 Locker getWriteOwnerLocker( Long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getWriteOwnerLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean validateOwnership__wrappee__base( Long nodeId, Locker locker, LockType type, boolean flushFromWaiters, MemoryBudget mb) throws DatabaseException { return true; }

	 protected boolean validateOwnership( Long nodeId, Locker locker, LockType type, boolean flushFromWaiters, MemoryBudget mb) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validateOwnership__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void dumpLockTable__wrappee__base( LockStats stats) throws DatabaseException { }

	 protected void dumpLockTable( LockStats stats) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLockTable__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
