package com.sleepycat.je.txn; 
import java.util.HashMap; 
import java.util.HashSet; 
import java.util.Hashtable; 
import java.util.Iterator; 
import java.util.Map; 
import java.util.Set; 
import com.sleepycat.je.Database; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.DeadlockException; 
import com.sleepycat.je.DbInternal; 
import com.sleepycat.je.LockNotGrantedException; 
import com.sleepycat.je.LockStats; 
import com.sleepycat.je.OperationStatus; 
import com.sleepycat.je.dbi.CursorImpl; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.tree.BIN; 
import com.sleepycat.je.tree.BINReference; 
import com.sleepycat.je.tree.Key; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  Locker {
	 private static final String DEBUG_NAME=Locker.class.getName();

	 protected EnvironmentImpl envImpl;

	 protected LockManager lockManager;

	 protected long id;

	 protected boolean readUncommittedDefault;

	 protected boolean defaultNoWait;

	 protected long lockTimeOutMillis;

	 private long txnTimeOutMillis;

	 private long txnStartMillis;

	 private Lock waitingFor;

	 protected Map handleLockToHandleMap;

	 protected Map handleToHandleLockMap;

	 protected Thread thread;

	 public Locker( EnvironmentImpl envImpl, boolean readUncommittedDefault, boolean noWait) throws DatabaseException { TxnManager txnManager=envImpl.getTxnManager(); this.id=generateId(txnManager); this.envImpl=envImpl; lockManager=txnManager.getLockManager(); this.readUncommittedDefault=readUncommittedDefault; this.waitingFor=null; defaultNoWait=noWait; lockTimeOutMillis=envImpl.getLockTimeout(); txnTimeOutMillis=envImpl.getTxnTimeout(); if (txnTimeOutMillis != 0) { txnStartMillis=System.currentTimeMillis(); } else { txnStartMillis=0; } thread=Thread.currentThread(); }

	 Locker(){ }

	 protected abstract long generateId( TxnManager txnManager);

	 public long getId(){ return id; }

	 public boolean getDefaultNoWait(){ return defaultNoWait; }

	 public synchronized long getLockTimeout(){ return lockTimeOutMillis; }

	 public synchronized void setLockTimeout( long timeOutMillis){ lockTimeOutMillis=timeOutMillis; }

	 public synchronized void setTxnTimeout( long timeOutMillis){ txnTimeOutMillis=timeOutMillis; txnStartMillis=System.currentTimeMillis(); }

	 public boolean isReadUncommittedDefault(){ return readUncommittedDefault; }

	 Lock getWaitingFor(){ return waitingFor; }

	 void setWaitingFor( Lock lock){ waitingFor=lock; }

	 void setOnlyAbortable(){ }

	 protected abstract void checkState( boolean ignoreCalledByAbort) throws DatabaseException ;

	 abstract LockResult lockInternal( long nodeId, LockType lockType, boolean noWait, DatabaseImpl database) throws DeadlockException, DatabaseException ;

	 public LockResult lock( long nodeId, LockType lockType, boolean noWait, DatabaseImpl database) throws LockNotGrantedException, DeadlockException, DatabaseException { LockResult result=lockInternal(nodeId,lockType,noWait,database); if (result.getLockGrant() == LockGrantType.DENIED) { throw new LockNotGrantedException("Non-blocking lock was denied."); } else { return result; } }

	 public LockResult nonBlockingLock( long nodeId, LockType lockType, DatabaseImpl database) throws DatabaseException { return lockInternal(nodeId,lockType,true,database); }

	 public void releaseLock( long nodeId) throws DatabaseException { lockManager.release(nodeId,this); }

	 public void demoteLock( long nodeId) throws DatabaseException { lockManager.demote(nodeId,this); }

	 public abstract boolean isTransactional();

	 public abstract boolean isSerializableIsolation();

	 public abstract boolean isReadCommittedIsolation();

	 public abstract Txn getTxnLocker();

	 public abstract Locker newNonTxnLocker() throws DatabaseException ;

	 public abstract void releaseNonTxnLocks() throws DatabaseException ;

	 public boolean sharesLocksWith( Locker other){ if (other instanceof BuddyLocker) { BuddyLocker buddy=(BuddyLocker)other; return buddy.getBuddy() == this; } else { return false; } }

	 public abstract void operationEnd() throws DatabaseException ;

	 public abstract void operationEnd( boolean operationOK) throws DatabaseException ;

	 public abstract void setHandleLockOwner( boolean operationOK, Database dbHandle, boolean dbIsClosing) throws DatabaseException ;

	 public void operationEnd( OperationStatus status) throws DatabaseException { operationEnd(status == OperationStatus.SUCCESS); }

	 public abstract void registerCursor( CursorImpl cursor) throws DatabaseException ;

	 public abstract void unRegisterCursor( CursorImpl cursor) throws DatabaseException ;

	 public abstract long getAbortLsn( long nodeId) throws DatabaseException ;

	 public abstract WriteLockInfo getWriteLockInfo( long nodeId) throws DatabaseException ;

	 abstract void addLock( Long nodeId, Lock lock, LockType type, LockGrantType grantStatus) throws DatabaseException ;

	 public abstract boolean createdNode( long nodeId) throws DatabaseException ;

	 abstract void removeLock( long nodeId, Lock lock) throws DatabaseException ;

	 abstract void moveWriteToReadLock( long nodeId, Lock lock);

	 boolean isTimedOut() throws DatabaseException { if (txnStartMillis != 0) { long diff=System.currentTimeMillis() - txnStartMillis; if (diff > txnTimeOutMillis) { return true; } } return false; }

	 public long getTxnTimeOut(){ return txnTimeOutMillis; }

	 long getTxnStartMillis(){ return txnStartMillis; }

	 void unregisterHandle( Database dbHandle){ if (handleToHandleLockMap != null) { handleToHandleLockMap.remove(dbHandle); } }

	 public void addToHandleMaps( Long handleLockId, Database databaseHandle){ Set dbHandleSet=null; if (handleLockToHandleMap == null) { handleLockToHandleMap=new Hashtable(); handleToHandleLockMap=new Hashtable(); } else { dbHandleSet=(Set)handleLockToHandleMap.get(handleLockId); } if (dbHandleSet == null) { dbHandleSet=new HashSet(); handleLockToHandleMap.put(handleLockId,dbHandleSet); } dbHandleSet.add(databaseHandle); handleToHandleLockMap.put(databaseHandle,handleLockId); }

	 public boolean isHandleLockTransferrable(){ return true; }

	 void transferHandleLockToHandle( Database dbHandle) throws DatabaseException { Locker holderTxn=new BasicLocker(envImpl); transferHandleLock(dbHandle,holderTxn,true); }

	 public void transferHandleLock( Database dbHandle, Locker destLocker, boolean demoteToRead) throws DatabaseException { if (DbInternal.dbGetDatabaseImpl(dbHandle) != null) { Long handleLockId=(Long)handleToHandleLockMap.get(dbHandle); if (handleLockId != null) { long nodeId=handleLockId.longValue(); lockManager.transfer(nodeId,this,destLocker,demoteToRead); destLocker.addToHandleMaps(handleLockId,dbHandle); Set dbHandleSet=(Set)handleLockToHandleMap.get(handleLockId); Iterator iter=dbHandleSet.iterator(); while (iter.hasNext()) { if (((Database)iter.next()) == dbHandle) { iter.remove(); break; } } if (dbHandleSet.size() == 0) { handleLockToHandleMap.remove(handleLockId); } DbInternal.dbSetHandleLocker(dbHandle,destLocker); } } }

	 protected void rememberHandleWriteLock( Long lockId){ }

	 public String toString(){ String className=getClass().getName(); className=className.substring(className.lastIndexOf('.') + 1); return Long.toString(id) + "_" + ((thread == null) ? "" : thread.getName())+ "_"+ className; }

	 public void dumpLockTable() throws DatabaseException { lockManager.dump(); }


}
