package com.sleepycat.je.txn; 
import java.util.HashSet; 
import java.util.Iterator; 
import java.util.Set; 
import com.sleepycat.je.Database; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.LockStats; 
import com.sleepycat.je.dbi.CursorImpl; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  BasicLocker  extends Locker {
	 private Lock ownedLock;

	 private Set ownedLockSet;

	 public BasicLocker( EnvironmentImpl env) throws DatabaseException { super(env,false,false); }

	 protected long generateId__wrappee__base( TxnManager txnManager){ return TxnManager.NULL_TXN_ID; }

	 protected long generateId( TxnManager txnManager){ t.in(Thread.currentThread().getStackTrace()[1].toString());	generateId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void checkState__wrappee__base( boolean ignoreCalledByAbort) throws DatabaseException { }

	 protected void checkState( boolean ignoreCalledByAbort) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkState__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 LockResult lockInternal__wrappee__base( long nodeId, LockType lockType, boolean noWait, DatabaseImpl database) throws DatabaseException {
synchronized (this) { checkState(false); } long timeout=0; boolean useNoWait=noWait || defaultNoWait; if (!useNoWait) {
synchronized (this) { timeout=lockTimeOutMillis; } } LockGrantType grant=lockManager.lock(nodeId,this,lockType,timeout,useNoWait,database); return new LockResult(grant,null); }

	 LockResult lockInternal( long nodeId, LockType lockType, boolean noWait, DatabaseImpl database) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	lockInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Locker getWriteOwnerLocker__wrappee__base( long nodeId) throws DatabaseException { return lockManager.getWriteOwnerLocker(new Long(nodeId)); }

	 public Locker getWriteOwnerLocker( long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getWriteOwnerLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getOwnerAbortLsn__wrappee__base( long nodeId) throws DatabaseException { Locker ownerTxn=lockManager.getWriteOwnerLocker(new Long(nodeId)); if (ownerTxn != null) { return ownerTxn.getAbortLsn(nodeId); } return DbLsn.NULL_LSN; }

	 public long getOwnerAbortLsn( long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getOwnerAbortLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isTransactional__wrappee__base(){ return false; }

	 public boolean isTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isSerializableIsolation__wrappee__base(){ return false; }

	 public boolean isSerializableIsolation(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isSerializableIsolation__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isReadCommittedIsolation__wrappee__base(){ return false; }

	 public boolean isReadCommittedIsolation(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isReadCommittedIsolation__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Txn getTxnLocker__wrappee__base(){ return null; }

	 public Txn getTxnLocker(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Locker newNonTxnLocker__wrappee__base() throws DatabaseException { return new BasicLocker(envImpl); }

	 public Locker newNonTxnLocker() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	newNonTxnLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void releaseNonTxnLocks__wrappee__base() throws DatabaseException { operationEnd(true); }

	 public void releaseNonTxnLocks() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	releaseNonTxnLocks__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void operationEnd__wrappee__base() throws DatabaseException { operationEnd(true); }

	 public void operationEnd() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	operationEnd__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void operationEnd__wrappee__base( boolean operationOK) throws DatabaseException { if (ownedLock != null) { lockManager.release(ownedLock,this); ownedLock=null; } if (ownedLockSet != null) { Iterator iter=ownedLockSet.iterator(); while (iter.hasNext()) { Lock l=(Lock)iter.next(); lockManager.release(l,this); } ownedLockSet.clear(); } }

	 public void operationEnd( boolean operationOK) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	operationEnd__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setHandleLockOwner__wrappee__base( boolean ignore, Database dbHandle, boolean dbIsClosing) throws DatabaseException { if (dbHandle != null) { if (!dbIsClosing) { transferHandleLockToHandle(dbHandle); } unregisterHandle(dbHandle); } }

	 public void setHandleLockOwner( boolean ignore, Database dbHandle, boolean dbIsClosing) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	setHandleLockOwner__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void registerCursor__wrappee__base( CursorImpl cursor) throws DatabaseException { }

	 public void registerCursor( CursorImpl cursor) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	registerCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void unRegisterCursor__wrappee__base( CursorImpl cursor) throws DatabaseException { }

	 public void unRegisterCursor( CursorImpl cursor) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	unRegisterCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getAbortLsn__wrappee__base( long nodeId) throws DatabaseException { return DbLsn.NULL_LSN; }

	 public long getAbortLsn( long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getAbortLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public WriteLockInfo getWriteLockInfo__wrappee__base( long nodeId) throws DatabaseException { return WriteLockInfo.basicWriteLockInfo; }

	 public WriteLockInfo getWriteLockInfo( long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getWriteLockInfo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void addLock__wrappee__base( Long nodeId, Lock lock, LockType type, LockGrantType grantStatus) throws DatabaseException { if (ownedLock == lock || (ownedLockSet != null && ownedLockSet.contains(lock))) { return; } if (ownedLock == null) { ownedLock=lock; } else { if (ownedLockSet == null) { ownedLockSet=new HashSet(); } ownedLockSet.add(lock); } }

	 void addLock( Long nodeId, Lock lock, LockType type, LockGrantType grantStatus) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	addLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void removeLock__wrappee__base( long nodeId, Lock lock) throws DatabaseException { if (lock == ownedLock) { ownedLock=null; } else if (ownedLockSet != null) { ownedLockSet.remove(lock); } }

	 void removeLock( long nodeId, Lock lock) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	removeLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean createdNode__wrappee__base( long nodeId) throws DatabaseException { return false; }

	 public boolean createdNode( long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	createdNode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void moveWriteToReadLock__wrappee__base( long nodeId, Lock lock){ }

	 void moveWriteToReadLock( long nodeId, Lock lock){ t.in(Thread.currentThread().getStackTrace()[1].toString());	moveWriteToReadLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
