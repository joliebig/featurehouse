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

	 protected long generateId( TxnManager txnManager){ return TxnManager.NULL_TXN_ID; }

	 protected void checkState( boolean ignoreCalledByAbort) throws DatabaseException { }

	 LockResult lockInternal( long nodeId, LockType lockType, boolean noWait, DatabaseImpl database) throws DatabaseException {
synchronized (this) { checkState(false); } long timeout=0; boolean useNoWait=noWait || defaultNoWait; if (!useNoWait) {
synchronized (this) { timeout=lockTimeOutMillis; } } LockGrantType grant=lockManager.lock(nodeId,this,lockType,timeout,useNoWait,database); return new LockResult(grant,null); }

	 public Locker getWriteOwnerLocker( long nodeId) throws DatabaseException { return lockManager.getWriteOwnerLocker(new Long(nodeId)); }

	 public long getOwnerAbortLsn( long nodeId) throws DatabaseException { Locker ownerTxn=lockManager.getWriteOwnerLocker(new Long(nodeId)); if (ownerTxn != null) { return ownerTxn.getAbortLsn(nodeId); } return DbLsn.NULL_LSN; }

	 public boolean isTransactional(){ return false; }

	 public boolean isSerializableIsolation(){ return false; }

	 public boolean isReadCommittedIsolation(){ return false; }

	 public Txn getTxnLocker(){ return null; }

	 public Locker newNonTxnLocker() throws DatabaseException { return new BasicLocker(envImpl); }

	 public void releaseNonTxnLocks() throws DatabaseException { operationEnd(true); }

	 public void operationEnd() throws DatabaseException { operationEnd(true); }

	 public void operationEnd( boolean operationOK) throws DatabaseException { if (ownedLock != null) { lockManager.release(ownedLock,this); ownedLock=null; } if (ownedLockSet != null) { Iterator iter=ownedLockSet.iterator(); while (iter.hasNext()) { Lock l=(Lock)iter.next(); lockManager.release(l,this); } ownedLockSet.clear(); } }

	 public void setHandleLockOwner( boolean ignore, Database dbHandle, boolean dbIsClosing) throws DatabaseException { if (dbHandle != null) { if (!dbIsClosing) { transferHandleLockToHandle(dbHandle); } unregisterHandle(dbHandle); } }

	 public void registerCursor( CursorImpl cursor) throws DatabaseException { }

	 public void unRegisterCursor( CursorImpl cursor) throws DatabaseException { }

	 public long getAbortLsn( long nodeId) throws DatabaseException { return DbLsn.NULL_LSN; }

	 public WriteLockInfo getWriteLockInfo( long nodeId) throws DatabaseException { return WriteLockInfo.basicWriteLockInfo; }

	 void addLock( Long nodeId, Lock lock, LockType type, LockGrantType grantStatus) throws DatabaseException { if (ownedLock == lock || (ownedLockSet != null && ownedLockSet.contains(lock))) { return; } if (ownedLock == null) { ownedLock=lock; } else { if (ownedLockSet == null) { ownedLockSet=new HashSet(); } ownedLockSet.add(lock); } }

	 void removeLock( long nodeId, Lock lock) throws DatabaseException { if (lock == ownedLock) { ownedLock=null; } else if (ownedLockSet != null) { ownedLockSet.remove(lock); } }

	 public boolean createdNode( long nodeId) throws DatabaseException { return false; }

	 void moveWriteToReadLock( long nodeId, Lock lock){ }


}
