package com.sleepycat.je.txn; 
import java.util.Set; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.LockStats; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.dbi.MemoryBudget; 
import de.ovgu.cide.jakutil.*; 
public  class  SyncedLockManager  extends LockManager {
	 public SyncedLockManager( EnvironmentImpl envImpl) throws DatabaseException { super(envImpl); }

	 protected LockAttemptResult attemptLock__wrappee__base( Long nodeId, Locker locker, LockType type, boolean nonBlockingRequest) throws DatabaseException { try { int lockTableIndex=getLockTableIndex(nodeId); this.hook782(nodeId,locker,type,nonBlockingRequest,lockTableIndex); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (LockAttemptResult)r.value; } }

	 protected LockAttemptResult attemptLock( Long nodeId, Locker locker, LockType type, boolean nonBlockingRequest) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	attemptLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected String makeTimeoutMsg__wrappee__base( String lockOrTxn, Locker locker, long nodeId, LockType type, LockGrantType grantType, Lock useLock, long timeout, long start, long now, DatabaseImpl database){ try { int lockTableIndex=getLockTableIndex(nodeId); this.hook783(lockOrTxn,locker,nodeId,type,grantType,useLock,timeout,start,now,database,lockTableIndex); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (String)r.value; } }

	 protected String makeTimeoutMsg( String lockOrTxn, Locker locker, long nodeId, LockType type, LockGrantType grantType, Lock useLock, long timeout, long start, long now, DatabaseImpl database){ t.in(Thread.currentThread().getStackTrace()[1].toString());	makeTimeoutMsg__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected Set releaseAndFindNotifyTargets__wrappee__base( long nodeId, Lock lock, Locker locker, boolean removeFromLocker) throws DatabaseException { try { long nid=nodeId; if (nid == -1) { nid=lock.getNodeId().longValue(); } int lockTableIndex=getLockTableIndex(nid); this.hook784(nodeId,lock,locker,removeFromLocker,lockTableIndex); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (Set)r.value; } }

	 protected Set releaseAndFindNotifyTargets( long nodeId, Lock lock, Locker locker, boolean removeFromLocker) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	releaseAndFindNotifyTargets__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void transfer__wrappee__base( long nodeId, Locker owningLocker, Locker destLocker, boolean demoteToRead) throws DatabaseException { int lockTableIndex=getLockTableIndex(nodeId); this.hook785(nodeId,owningLocker,destLocker,demoteToRead,lockTableIndex); }

	 void transfer( long nodeId, Locker owningLocker, Locker destLocker, boolean demoteToRead) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	transfer__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void transferMultiple__wrappee__base( long nodeId, Locker owningLocker, Locker[] destLockers) throws DatabaseException { int lockTableIndex=getLockTableIndex(nodeId); this.hook786(nodeId,owningLocker,destLockers,lockTableIndex); }

	 void transferMultiple( long nodeId, Locker owningLocker, Locker[] destLockers) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	transferMultiple__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void demote__wrappee__base( long nodeId, Locker locker) throws DatabaseException { int lockTableIndex=getLockTableIndex(nodeId); this.hook787(nodeId,locker,lockTableIndex); }

	 void demote( long nodeId, Locker locker) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	demote__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isLocked__wrappee__base( Long nodeId){ try { int lockTableIndex=getLockTableIndex(nodeId); this.hook788(nodeId,lockTableIndex); throw ReturnHack.returnBoolean; } catch ( ReturnBoolean r) { return r.value; } }

	 boolean isLocked( Long nodeId){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isLocked__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isOwner__wrappee__base( Long nodeId, Locker locker, LockType type){ try { int lockTableIndex=getLockTableIndex(nodeId); this.hook789(nodeId,locker,type,lockTableIndex); throw ReturnHack.returnBoolean; } catch ( ReturnBoolean r) { return r.value; } }

	 boolean isOwner( Long nodeId, Locker locker, LockType type){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isOwner__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isWaiter__wrappee__base( Long nodeId, Locker locker){ try { int lockTableIndex=getLockTableIndex(nodeId); this.hook790(nodeId,locker,lockTableIndex); throw ReturnHack.returnBoolean; } catch ( ReturnBoolean r) { return r.value; } }

	 boolean isWaiter( Long nodeId, Locker locker){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isWaiter__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 int nWaiters__wrappee__base( Long nodeId){ try { int lockTableIndex=getLockTableIndex(nodeId); this.hook791(nodeId,lockTableIndex); throw ReturnHack.returnInt; } catch ( ReturnInt r) { return r.value; } }

	 int nWaiters( Long nodeId){ t.in(Thread.currentThread().getStackTrace()[1].toString());	nWaiters__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 int nOwners__wrappee__base( Long nodeId){ try { int lockTableIndex=getLockTableIndex(nodeId); this.hook792(nodeId,lockTableIndex); throw ReturnHack.returnInt; } catch ( ReturnInt r) { return r.value; } }

	 int nOwners( Long nodeId){ t.in(Thread.currentThread().getStackTrace()[1].toString());	nOwners__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 Locker getWriteOwnerLocker__wrappee__base( Long nodeId) throws DatabaseException { try { int lockTableIndex=getLockTableIndex(nodeId); this.hook793(nodeId,lockTableIndex); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (Locker)r.value; } }

	 Locker getWriteOwnerLocker( Long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getWriteOwnerLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean validateOwnership__wrappee__base( Long nodeId, Locker locker, LockType type, boolean flushFromWaiters, MemoryBudget mb) throws DatabaseException { try { int lockTableIndex=getLockTableIndex(nodeId); this.hook794(nodeId,locker,type,flushFromWaiters,mb,lockTableIndex); throw ReturnHack.returnBoolean; } catch ( ReturnBoolean r) { return r.value; } }

	 protected boolean validateOwnership( Long nodeId, Locker locker, LockType type, boolean flushFromWaiters, MemoryBudget mb) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validateOwnership__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void dumpLockTable__wrappee__base( LockStats stats) throws DatabaseException { for (int i=0; i < nLockTables; i++) { this.hook795(stats,i); } }

	 protected void dumpLockTable( LockStats stats) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLockTable__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook782__wrappee__base( Long nodeId, Locker locker, LockType type, boolean nonBlockingRequest, int lockTableIndex) throws DatabaseException { throw new ReturnObject(attemptLockInternal(nodeId,locker,type,nonBlockingRequest,lockTableIndex)); }

	 protected void hook782( Long nodeId, Locker locker, LockType type, boolean nonBlockingRequest, int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook782__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook783__wrappee__base( String lockOrTxn, Locker locker, long nodeId, LockType type, LockGrantType grantType, Lock useLock, long timeout, long start, long now, DatabaseImpl database, int lockTableIndex){ throw new ReturnObject(makeTimeoutMsgInternal(lockOrTxn,locker,nodeId,type,grantType,useLock,timeout,start,now,database)); }

	 protected void hook783( String lockOrTxn, Locker locker, long nodeId, LockType type, LockGrantType grantType, Lock useLock, long timeout, long start, long now, DatabaseImpl database, int lockTableIndex){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook783__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook784__wrappee__base( long nodeId, Lock lock, Locker locker, boolean removeFromLocker, int lockTableIndex) throws DatabaseException { throw new ReturnObject(releaseAndFindNotifyTargetsInternal(nodeId,lock,locker,removeFromLocker,lockTableIndex)); }

	 protected void hook784( long nodeId, Lock lock, Locker locker, boolean removeFromLocker, int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook784__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook785__wrappee__base( long nodeId, Locker owningLocker, Locker destLocker, boolean demoteToRead, int lockTableIndex) throws DatabaseException { transferInternal(nodeId,owningLocker,destLocker,demoteToRead,lockTableIndex); }

	 protected void hook785( long nodeId, Locker owningLocker, Locker destLocker, boolean demoteToRead, int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook785__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook786__wrappee__base( long nodeId, Locker owningLocker, Locker[] destLockers, int lockTableIndex) throws DatabaseException { transferMultipleInternal(nodeId,owningLocker,destLockers,lockTableIndex); }

	 protected void hook786( long nodeId, Locker owningLocker, Locker[] destLockers, int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook786__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook787__wrappee__base( long nodeId, Locker locker, int lockTableIndex) throws DatabaseException { demoteInternal(nodeId,locker,lockTableIndex); }

	 protected void hook787( long nodeId, Locker locker, int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook787__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook788__wrappee__base( Long nodeId, int lockTableIndex){ throw new ReturnBoolean(isLockedInternal(nodeId,lockTableIndex)); }

	 protected void hook788( Long nodeId, int lockTableIndex){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook788__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook789__wrappee__base( Long nodeId, Locker locker, LockType type, int lockTableIndex){ throw new ReturnBoolean(isOwnerInternal(nodeId,locker,type,lockTableIndex)); }

	 protected void hook789( Long nodeId, Locker locker, LockType type, int lockTableIndex){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook789__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook790__wrappee__base( Long nodeId, Locker locker, int lockTableIndex){ throw new ReturnBoolean(isWaiterInternal(nodeId,locker,lockTableIndex)); }

	 protected void hook790( Long nodeId, Locker locker, int lockTableIndex){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook790__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook791__wrappee__base( Long nodeId, int lockTableIndex){ throw new ReturnInt(nWaitersInternal(nodeId,lockTableIndex)); }

	 protected void hook791( Long nodeId, int lockTableIndex){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook791__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook792__wrappee__base( Long nodeId, int lockTableIndex){ throw new ReturnInt(nOwnersInternal(nodeId,lockTableIndex)); }

	 protected void hook792( Long nodeId, int lockTableIndex){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook792__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook793__wrappee__base( Long nodeId, int lockTableIndex) throws DatabaseException { throw new ReturnObject(getWriteOwnerLockerInternal(nodeId,lockTableIndex)); }

	 protected void hook793( Long nodeId, int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook793__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook794__wrappee__base( Long nodeId, Locker locker, LockType type, boolean flushFromWaiters, MemoryBudget mb, int lockTableIndex) throws DatabaseException { throw new ReturnBoolean(validateOwnershipInternal(nodeId,locker,type,flushFromWaiters,mb,lockTableIndex)); }

	 protected void hook794( Long nodeId, Locker locker, LockType type, boolean flushFromWaiters, MemoryBudget mb, int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook794__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook795__wrappee__base( LockStats stats, int i) throws DatabaseException { dumpLockTableInternal(stats,i); }

	 protected void hook795( LockStats stats, int i) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook795__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
